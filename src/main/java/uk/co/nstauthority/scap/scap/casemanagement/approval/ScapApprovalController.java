package uk.co.nstauthority.scap.scap.casemanagement.approval;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_APPROVED;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_CLOSED_OUT;

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
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.endpointvalidation.annotations.UserHasAnyPermission;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.notify.ScapEmailService;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryModelAndViewGenerator;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;

@Controller
@RequestMapping("{scapId}/")
@UserHasAnyPermission(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
public class ScapApprovalController {

  private final CaseEventService caseEventService;

  private final ControllerHelperService controllerHelperService;

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;

  private final ScapApprovalFormValidator scapApprovalFormValidator;

  private final SupportingDocumentService supportingDocumentService;
  private final ScapEmailService scapEmailService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  @Autowired
  public ScapApprovalController(CaseEventService caseEventService,
                                ControllerHelperService controllerHelperService,
                                ScapDetailService scapDetailService,
                                ScapSummaryViewService scapSummaryViewService,
                                OrganisationGroupService organisationGroupService,
                                ScapApprovalFormValidator scapApprovalFormValidator,
                                SupportingDocumentService supportingDocumentService,
                                ScapEmailService scapEmailService, TeamService teamService,
                                UserDetailService userDetailService) {
    this.caseEventService = caseEventService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.scapApprovalFormValidator = scapApprovalFormValidator;
    this.supportingDocumentService = supportingDocumentService;
    this.scapEmailService = scapEmailService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
  }

  @SuppressWarnings("ConstantConditions")
  // Hide IntelliJ warning when ReverseRouting with null parameters
  @PostMapping(params = CaseEventAction.APPROVED)
  public ModelAndView saveScapApprovalForm(@PathVariable("scapId") ScapId scapId,
                                           @RequestParam(CaseEventAction.APPROVED) String caseEventAction,
                                           @RequestParam("Approve-scap-Panel") Boolean slideOutPanelOpen,
                                           @ModelAttribute("scapApprovalForm") ScapApprovalForm scapApprovalForm,
                                           BindingResult bindingResult) {
    scapApprovalFormValidator.validate(scapApprovalForm, bindingResult);

    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var existingFiles = supportingDocumentService.getFileUploadFormListForScapDetailAndType(
        scapDetail,
        SupportingDocumentType.APPROVAL_DOCUMENT);

    var generator =
        ScapSummaryModelAndViewGenerator.generator(scapDetail,
                scapSummary,
                supportingDocumentService)
            .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
            .withScapApprovalForm(scapApprovalForm)
            .withScapApprovalDocuments(existingFiles)
            .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
            .withScapVersions(scapDetailService.getAllVersionsForUser(scapDetail.getScap()))
            .withUpdatePermission(teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail()))
            .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId));
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        generator.generate(),
        scapApprovalForm,
        () -> {
          caseEventService.recordNewEvent(
              scapApprovalForm.getProjectClosedOut().equals(YesNo.YES) ? SCAP_CLOSED_OUT : SCAP_APPROVED,
              scapId,
              scapDetail.getVersionNumber(),
              scapApprovalForm.getApprovalComments().getInputValue());
          if (scapApprovalForm.getProjectClosedOut().equals(YesNo.YES)) {
            scapDetailService.closeOutScap(scapDetail);
          } else {
            scapDetailService.approveScap(scapDetail);
          }
          scapEmailService.sendScapApprovalEmails(
              scapDetail, scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary)
          );

          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
