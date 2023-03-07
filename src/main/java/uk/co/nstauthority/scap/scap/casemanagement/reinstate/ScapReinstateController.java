package uk.co.nstauthority.scap.scap.casemanagement.reinstate;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_REINSTATED;

import java.util.Optional;
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
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryModelAndViewGenerator;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.util.NotificationBannerUtils;

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
  private final SupportingDocumentService supportingDocumentService;
  private final ScapService scapService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  @Autowired
  public ScapReinstateController(CaseEventService caseEventService,
                                 ControllerHelperService controllerHelperService,
                                 ScapDetailService scapDetailService,
                                 ScapSummaryViewService scapSummaryViewService,
                                 OrganisationGroupService organisationGroupService,
                                 ScapReinstateFormValidator scapReinstateFormValidator,
                                 SupportingDocumentService supportingDocumentService,
                                 ScapService scapService,
                                 TeamService teamService,
                                 UserDetailService userDetailService) {
    this.caseEventService = caseEventService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.scapReinstateFormValidator = scapReinstateFormValidator;
    this.supportingDocumentService = supportingDocumentService;
    this.scapService = scapService;
    this.teamService = teamService;
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

    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator =
        ScapSummaryModelAndViewGenerator.generator(scapDetail,
                scapSummary,
                supportingDocumentService)
            .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
            .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
            .withScapReinstateForm(scapReinstateForm)
            .withScapVersions(scapDetailService.getAllVersionsForUser(scapDetail.getScap()))
            .withUpdatePermission(teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail()))
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
              scapId,
              scapDetail.getVersionNumber(),
              scapReinstateForm.getReinstateComments().getInputValue());
          var modelAndView =  ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));

          NotificationBannerUtils.successBannerRedirect(
              "Success",
              new NotificationBannerBodyLine(
                  "%s has been reinstated".formatted(scapDetail.getScap().getReference()), "govuk-!-font-weight-bold"
              ), redirectAttributes);
          return modelAndView;
        });
  }
}
