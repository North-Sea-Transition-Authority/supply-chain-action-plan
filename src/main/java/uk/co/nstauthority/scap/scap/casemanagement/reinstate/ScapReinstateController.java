package uk.co.nstauthority.scap.scap.casemanagement.reinstate;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_REINSTATED;

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
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryModelAndViewGenerator;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.util.SuccessBannerUtil;

@Controller
@RequestMapping("{scapId}/")
@UserHasAnyPermission(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = {ScapDetailStatus.CLOSED_OUT, ScapDetailStatus.WITHDRAWN})
public class ScapReinstateController {

  private final CaseEventService caseEventService;

  private final ControllerHelperService controllerHelperService;

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;
  private final ScapReinstateFormValidator scapReinstateFormValidator;
  private final CaseEventDocumentService caseEventDocumentService;
  private final ScapService scapService;

  private final TeamMemberService teamMemberService;

  private final UserDetailService userDetailService;

  @Autowired
  public ScapReinstateController(CaseEventService caseEventService,
                                 ControllerHelperService controllerHelperService,
                                 ScapDetailService scapDetailService,
                                 ScapSummaryViewService scapSummaryViewService,
                                 OrganisationGroupService organisationGroupService,
                                 ScapReinstateFormValidator scapReinstateFormValidator,
                                 CaseEventDocumentService caseEventDocumentService,
                                 ScapService scapService,
                                 TeamMemberService teamMemberService,
                                 UserDetailService userDetailService) {
    this.caseEventService = caseEventService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.scapReinstateFormValidator = scapReinstateFormValidator;
    this.caseEventDocumentService = caseEventDocumentService;
    this.scapService = scapService;
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
  }

  @PostMapping(params = CaseEventAction.REINSTATE)
  public ModelAndView reinstateScap(@PathVariable("scapId") ScapId scapId,
                                    @RequestParam(CaseEventAction.REINSTATE) String caseEventAction,
                                    @RequestParam("Withdraw-scap-panel") Boolean slideOutPanelOpen,
                                    @ModelAttribute("scapReinstateForm") ScapReinstateForm scapReinstateForm,
                                    BindingResult bindingResult,
                                    RedirectAttributes redirectAttributes) {
    scapReinstateFormValidator.validate(scapReinstateForm, bindingResult);

    var scapDetail = scapDetailService.getActionableScapDetail(scapId, userDetailService.getUserDetail());
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator =
        ScapSummaryModelAndViewGenerator.generator(scapDetail,
                scapSummary,
                caseEventDocumentService)
            .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
            .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
            .withScapReinstateForm(scapReinstateForm)
            .withScapVersions(scapDetailService.getAllVersionsForUser(scapDetail.getScap()))
            .withIsUpdateable(
                teamMemberService.getAllPermissionsForUser(userDetailService.getUserDetail().wuaId()),
                scapDetail.getStatus())
            .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId));
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        generator.generate(),
        scapReinstateForm,
        () -> {
          scapDetailService.reinstateScap(scapService.getScapById(scapId));
          caseEventService.recordNewEvent(
              SCAP_REINSTATED,
              scapDetail,
              scapDetail.getVersionNumber(),
              scapReinstateForm.getReinstateComments().getInputValue());
          SuccessBannerUtil.add(
              redirectAttributes,
              "%s has been reinstated".formatted(scapDetail.getScap().getReference())
          );
          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
