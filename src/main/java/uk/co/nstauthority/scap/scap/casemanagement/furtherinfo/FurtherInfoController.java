package uk.co.nstauthority.scap.scap.casemanagement.furtherinfo;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.LocalDate;
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
@ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
public class FurtherInfoController {

  private final CaseEventService caseEventService;

  private final ScapDetailService scapDetailService;

  private final ControllerHelperService controllerHelperService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;

  private final FurtherInfoRequestFormValidator furtherInfoRequestFormValidator;

  private final SupportingDocumentService supportingDocumentService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  private final UpdateRequestService updateRequestService;

  @Autowired
  public FurtherInfoController(CaseEventService caseEventService,
                               ScapDetailService scapDetailService,
                               ControllerHelperService controllerHelperService,
                               ScapSummaryViewService scapSummaryViewService,
                               OrganisationGroupService organisationGroupService,
                               FurtherInfoRequestFormValidator furtherInfoRequestFormValidator,
                               SupportingDocumentService supportingDocumentService,
                               TeamService teamService,
                               UserDetailService userDetailService,
                               UpdateRequestService updateRequestService) {
    this.caseEventService = caseEventService;
    this.scapDetailService = scapDetailService;
    this.controllerHelperService = controllerHelperService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.furtherInfoRequestFormValidator = furtherInfoRequestFormValidator;
    this.supportingDocumentService = supportingDocumentService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
    this.updateRequestService = updateRequestService;
  }

  @PostMapping(params = CaseEventAction.INFO_REQUESTED)
  public ModelAndView saveInfoRequestedForm(@PathVariable("scapId") ScapId scapId,
                                            @RequestParam(CaseEventAction.INFO_REQUESTED) String caseEventAction,
                                            @RequestParam("Info-Request-Panel") Boolean slideoutPanelOpen,
                                            @ModelAttribute("infoRequestForm") FurtherInfoRequestForm infoRequestForm,
                                            BindingResult bindingResult) {
    furtherInfoRequestFormValidator.validate(infoRequestForm, bindingResult);

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
        .withFurtherInfoRequestFrom(infoRequestForm)
        .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
        .withScapVersions(scapDetailService.getAllVersionsForUser(scapDetail.getScap()))
        .withUpdatePermission(teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail()))
        .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId));
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(bindingResult,
        generator.generate(),
        infoRequestForm,
        () -> {
          updateRequestService.createUpdateRequest(scapDetail, UpdateRequestType.FURTHER_INFORMATION, LocalDate.now());
          caseEventService.recordNewEvent(CaseEventSubject.FURTHER_INFO_REQUESTED,
              scapDetail,
              scapDetail.getVersionNumber(),
              infoRequestForm.getInfoRequest().getInputValue());
          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
