package uk.co.nstauthority.scap.scap.casemanagement.consultationresponse;

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

@Controller
@RequestMapping("{scapId}/")
@UserHasAnyPermission(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
public class ConsultationResponseController {

  private final CaseEventService caseEventService;

  private final ControllerHelperService controllerHelperService;

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;

  private final ConsultationResponseFormValidator consultationResponseFormValidator;

  private final SupportingDocumentService supportingDocumentService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  @Autowired
  public ConsultationResponseController(CaseEventService caseEventService,
                                        ControllerHelperService controllerHelperService,
                                        ScapDetailService scapDetailService,
                                        ScapSummaryViewService scapSummaryViewService,
                                        OrganisationGroupService organisationGroupService,
                                        ConsultationResponseFormValidator consultationRequestFormValidator,
                                        SupportingDocumentService supportingDocumentService,
                                        TeamService teamService,
                                        UserDetailService userDetailService) {
    this.caseEventService = caseEventService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.consultationResponseFormValidator = consultationRequestFormValidator;
    this.supportingDocumentService = supportingDocumentService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
  }

  @PostMapping(params = CaseEventAction.CONSULTATION_RESPONSE)
  public ModelAndView saveConsultationResponseForm(@PathVariable("scapId") ScapId scapId,
                                                  @RequestParam(CaseEventAction.CONSULTATION_RESPONSE) String caseEventAction,
                                                  @RequestParam("Consultation-Response-Panel") Boolean slideOutPanelOpen,
                                                  @ModelAttribute("form")
                                                    ConsultationResponseForm consultationResponseForm,
                                                  BindingResult bindingResult) {
    consultationResponseFormValidator.validate(consultationResponseForm, bindingResult);
    var scapDetail = scapDetailService.getActionableScapDetail(scapId, userDetailService.getUserDetail());
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator = ScapSummaryModelAndViewGenerator.generator(
            scapDetail,
            scapSummary,
            supportingDocumentService)
        .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
        .withConsultationResponseForm(consultationResponseForm)
        .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
        .withScapVersions(scapDetailService.getAllVersionsForUser(scapDetail.getScap()))
        .withUpdatePermission(teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail()))
        .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId));
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        generator.generate(),
        consultationResponseForm,
        () -> {
          caseEventService.recordNewEvent(CaseEventSubject.SCAP_CONSULTATION_RESPONSE,
              scapDetail,
              scapDetail.getVersionNumber(),
              consultationResponseForm.getResponseComments().getInputValue());
          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
