package uk.co.nstauthority.scap.scap.casemanagement.updaterequest;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

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
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryModelAndViewGenerator;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

@Controller
@RequestMapping("{scapId}/")
@UserHasAnyPermission(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = {ScapDetailStatus.SUBMITTED, ScapDetailStatus.APPROVED})
public class UpdateRequestController {

  private final CaseEventService caseEventService;

  private final ScapDetailService scapDetailService;

  private final ControllerHelperService controllerHelperService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;

  private final UpdateRequestFormValidator updateRequestFormValidator;

  private final SupportingDocumentService supportingDocumentService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  private final UpdateRequestService updateRequestService;

  @Autowired
  public UpdateRequestController(CaseEventService caseEventService,
                                 ScapDetailService scapDetailService,
                                 ControllerHelperService controllerHelperService,
                                 ScapSummaryViewService scapSummaryViewService,
                                 OrganisationGroupService organisationGroupService,
                                 UpdateRequestFormValidator updateRequestFormValidator,
                                 SupportingDocumentService supportingDocumentService,
                                 TeamService teamService,
                                 UserDetailService userDetailService,
                                 UpdateRequestService updateRequestService) {
    this.caseEventService = caseEventService;
    this.scapDetailService = scapDetailService;
    this.controllerHelperService = controllerHelperService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.updateRequestFormValidator = updateRequestFormValidator;
    this.supportingDocumentService = supportingDocumentService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
    this.updateRequestService = updateRequestService;
  }

  @PostMapping(params = CaseEventAction.UPDATE_REQUESTED)
  public ModelAndView saveInfoRequestedForm(@PathVariable("scapId") ScapId scapId,
                                            @RequestParam(CaseEventAction.UPDATE_REQUESTED) String caseEventAction,
                                            @RequestParam("Update-Request-Panel") Boolean slideoutPanelOpen,
                                            @ModelAttribute("updateRequestForm") UpdateRequestForm updateRequestForm,
                                            BindingResult bindingResult) {
    updateRequestFormValidator.validate(updateRequestForm, bindingResult);

    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator = ScapSummaryModelAndViewGenerator.generator(
                scapDetail,
                scapSummary,
                supportingDocumentService)
        .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
        .withUpdateRequestForm(updateRequestForm)
        .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
        .withUpdatePermission(teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail()))
        .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId));
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(bindingResult,
        generator.generate(),
        updateRequestForm,
        () -> {
          var dueDate = updateRequestForm.getDueDate().getAsLocalDate().orElseThrow(
              () -> new ScapEntityNotFoundException("Could not find Due Date")
          );
          updateRequestService.createUpdateRequest(scapDetail, UpdateRequestType.UPDATE, dueDate);
          caseEventService.recordNewEvent(CaseEventSubject.SCAP_UPDATE_REQUESTED,
              scapDetail,
              scapDetail.getVersionNumber(),
              updateRequestForm.getInfoRequest().getInputValue(),
              updateRequestForm.getDueDate().getAsLocalDate().get());
          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
