package uk.co.nstauthority.scap.scap.casemanagement.consultationrequest;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType.CONSULTATION_REPORT;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequired;
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
@PermissionsRequired(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
public class ConsultationRequestController {

  private final CaseEventService caseEventService;

  private final ControllerHelperService controllerHelperService;

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;

  private final ConsultationRequestFormValidator consultationRequestFormValidator;

  private final SupportingDocumentService supportingDocumentService;

  @Autowired
  public ConsultationRequestController(CaseEventService caseEventService,
                                       ControllerHelperService controllerHelperService,
                                       ScapDetailService scapDetailService,
                                       ScapSummaryViewService scapSummaryViewService,
                                       OrganisationGroupService organisationGroupService,
                                       ConsultationRequestFormValidator consultationRequestFormValidator,
                                       SupportingDocumentService supportingDocumentService) {
    this.caseEventService = caseEventService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.consultationRequestFormValidator = consultationRequestFormValidator;
    this.supportingDocumentService = supportingDocumentService;
  }

  @PostMapping(params = CaseEventAction.CONSULTATION_REQUESTED)
  public ModelAndView saveConsultationRequestForm(@PathVariable("scapId") ScapId scapId,
                                                  @RequestParam(CaseEventAction.CONSULTATION_REQUESTED) String caseEventAction,
                                                  @RequestParam("Consultation-Request-Panel") Boolean slideOutPanelOpen,
                                                  @ModelAttribute("consultationRequestForm")
                                                    ConsultationRequestForm consultationRequestForm,
                                                  BindingResult bindingResult) {
    consultationRequestFormValidator.validate(consultationRequestForm, bindingResult);

    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator = ScapSummaryModelAndViewGenerator.generator(
                scapDetail,
                scapSummary,
                supportingDocumentService.buildFileUploadTemplate(scapId, CONSULTATION_REPORT))
            .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
            .withConsultationRequestForm(consultationRequestForm);
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        generator.generate(),
        consultationRequestForm,
        () -> {
          caseEventService.recordNewEvent(CaseEventSubject.SCAP_CONSULTATION_REQUESTED,
              scapId,
              scapDetail.getVersionNumber(),
              consultationRequestForm.getRequestComments().getInputValue());
          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
