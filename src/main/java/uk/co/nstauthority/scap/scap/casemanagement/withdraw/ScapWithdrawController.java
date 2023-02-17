package uk.co.nstauthority.scap.scap.casemanagement.withdraw;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_WITHDRAWN;
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
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.notify.ScapEmailService;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequired;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryModelAndViewGenerator;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.util.NotificationBannerUtils;

@Controller
@RequestMapping("{scapId}/")
@PermissionsRequired(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
public class ScapWithdrawController {

  private final CaseEventService caseEventService;

  private final ControllerHelperService controllerHelperService;

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;
  private final ScapWithdrawalFormValidator scapWithdrawalFormValidator;
  private final SupportingDocumentService supportingDocumentService;
  private final ScapEmailService scapEmailService;

  @Autowired
  public ScapWithdrawController(CaseEventService caseEventService,
                                ControllerHelperService controllerHelperService,
                                ScapDetailService scapDetailService,
                                ScapSummaryViewService scapSummaryViewService,
                                OrganisationGroupService organisationGroupService,
                                ScapWithdrawalFormValidator scapWithdrawalFormValidator,
                                SupportingDocumentService supportingDocumentService,
                                ScapEmailService scapEmailService) {
    this.caseEventService = caseEventService;
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.scapWithdrawalFormValidator = scapWithdrawalFormValidator;
    this.supportingDocumentService = supportingDocumentService;
    this.scapEmailService = scapEmailService;
  }

  @PostMapping(params = CaseEventAction.WITHDRAWN)
  public ModelAndView withdrawScap(@PathVariable("scapId") ScapId scapId,
                                   @RequestParam(CaseEventAction.WITHDRAWN) String caseEventAction,
                                   @RequestParam("Withdraw-scap-panel") Boolean slideOutPanelOpen,
                                   @ModelAttribute("scapWithdrawForm") ScapWithdrawalForm scapWithdrawalForm,
                                   BindingResult bindingResult,
                                   RedirectAttributes redirectAttributes) {
    scapWithdrawalFormValidator.validate(scapWithdrawalForm, bindingResult);

    var scapDetail = scapDetailService.getLatestSubmittedScapDetail(scapId);
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator =
        ScapSummaryModelAndViewGenerator.generator(scapDetail,
                scapSummary,
                supportingDocumentService.buildFileUploadTemplate(scapId, CONSULTATION_REPORT))
            .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
            .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
            .withScapWithdrawalForm(scapWithdrawalForm)
            .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId));
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        generator.generate(),
        scapWithdrawalForm,
        () -> {
          scapDetailService.withdrawScap(scapDetail);
          scapEmailService.sendScapWithdrawalEmails(scapDetail);
          caseEventService.recordNewEvent(
              SCAP_WITHDRAWN,
              scapId,
              scapDetail.getVersionNumber(),
              scapWithdrawalForm.getWithdrawComments().getInputValue());
          var modelAndView =  ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));

          NotificationBannerUtils.successBannerRedirect(
              "Success",
              new NotificationBannerBodyLine(
                  "%s has been withdrawn".formatted(scapDetail.getScap().getReference()), "govuk-!-font-weight-bold"
              ), redirectAttributes);
          return modelAndView;
        });
  }
}
