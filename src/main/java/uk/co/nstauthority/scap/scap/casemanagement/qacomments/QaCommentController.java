package uk.co.nstauthority.scap.scap.casemanagement.qacomments;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.endpointvalidation.annotations.UserHasAnyPermission;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventDocumentService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryModelAndViewGenerator;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.util.NotificationBannerUtils;

@Controller
@RequestMapping("{scapId}/")
@UserHasAnyPermission(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
public class QaCommentController {

  private final CaseEventService caseEventService;

  private final ControllerHelperService controllerHelperService;

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;

  private final QaCommentFormValidator qaCommentFormValidator;

  private final CaseEventDocumentService caseEventDocumentService;

  private final TeamMemberService teamMemberService;

  private final UserDetailService userDetailService;

  @Autowired
  public QaCommentController(CaseEventService caseEventService,
                             ControllerHelperService controllerHelperService,
                             ScapDetailService scapDetailService,
                             ScapSummaryViewService scapSummaryViewService,
                             OrganisationGroupService organisationGroupService,
                             QaCommentFormValidator qaCommentFormValidator,
                             CaseEventDocumentService caseEventDocumentService,
                             TeamMemberService teamMemberService,
                             UserDetailService userDetailService) {
    this.caseEventService = caseEventService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.qaCommentFormValidator = qaCommentFormValidator;
    this.caseEventDocumentService = caseEventDocumentService;
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
  }

  @PostMapping(params = CaseEventAction.QA)
  public ModelAndView saveQaCommentForm(@PathVariable("scapId") ScapId scapId,
                                        @RequestParam(CaseEventAction.QA) String caseEventAction,
                                        @RequestParam("Qa-Panel") Boolean slideOutPanelOpen,
                                        @ModelAttribute("qaForm") QaCommentForm qaCommentForm,
                                        BindingResult bindingResult,
                                        RedirectAttributes redirectAttributes) {

    qaCommentFormValidator.validate(qaCommentForm, bindingResult);
    var scapDetail = scapDetailService.getActionableScapDetail(scapId, userDetailService.getUserDetail());
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator = ScapSummaryModelAndViewGenerator.generator(
                scapDetail,
                scapSummary,
                caseEventDocumentService)
        .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
        .withQaCommentForm(qaCommentForm)
        .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
        .withScapVersions(scapDetailService.getAllVersionsForUser(scapDetail.getScap()))
        .withIsUpdateable(
            teamMemberService.getAllPermissionsForUser(userDetailService.getUserDetail().wuaId()),
            scapDetail.getStatus())
        .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId));
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        generator.generate(),
        qaCommentForm,
        () -> {
          caseEventService.recordNewEvent(CaseEventSubject.QA_COMMENT,
              scapDetail,
              scapDetail.getVersionNumber(),
              qaCommentForm.getQaComments().getInputValue());

          NotificationBannerUtils.successBannerRedirect(
              "Added QA comment to %s".formatted(scapDetail.getScap().getReference()),
              Collections.emptyList(),
              redirectAttributes);
          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
