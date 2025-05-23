package uk.co.nstauthority.scap.scap.submit;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.SCAP_SUBMITTED;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.error.exception.ScapBadRequestException;
import uk.co.nstauthority.scap.feedback.FeedbackController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.notify.ScapEmailService;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;

@Controller
@RequestMapping("{scapId}/submit")
@HasAnyPermissionForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
class ScapSubmissionController {

  static final String INVALID_SCAP_ERROR_MESSAGE = """
      You cannot submit a SCAP until all sections shown on the task list are completed
      """;

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ScapSummaryViewService scapSummaryViewService;
  private final CaseEventService caseEventService;
  private final ReviewAndSubmitFormService reviewAndSubmitFormService;
  private final ControllerHelperService controllerHelperService;
  private final ScapEmailService scapEmailService;
  private final ScapSubmissionService scapSubmissionService;

  private final UpdateRequestService updateRequestService;

  @Autowired
  ScapSubmissionController(ScapService scapService,
                           ScapDetailService scapDetailService,
                           ScapSummaryViewService scapSummaryViewService,
                           CaseEventService caseEventService,
                           ReviewAndSubmitFormService reviewAndSubmitFormService,
                           ControllerHelperService controllerHelperService,
                           UpdateRequestService updateRequestService,
                           ScapEmailService scapEmailService,
                           ScapSubmissionService scapSubmissionService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.caseEventService = caseEventService;
    this.reviewAndSubmitFormService = reviewAndSubmitFormService;
    this.controllerHelperService = controllerHelperService;
    this.scapEmailService = scapEmailService;
    this.updateRequestService = updateRequestService;
    this.scapSubmissionService = scapSubmissionService;
  }

  @GetMapping
  ModelAndView renderScapSubmissionConfirmation(@PathVariable("scapId") ScapId scapId) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);
    var form = reviewAndSubmitFormService.getForm(scapDetail);

    var updateRequest = updateRequestService.findNextDueUpdate(scapId)
        .map(request -> request.getCaseEvent().getComments())
        .orElse(null);

    return scapSubmissionConfirmationModelAndView(scapId, scapDetail)
        .addObject("form", form)
        .addObject("updateText", updateRequest);
  }

  @GetMapping("/success")
  @ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
  ModelAndView renderScapSubmissionSuccess(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);

    return new ModelAndView("scap/scap/submit/submissionSuccess")
        .addObject("workAreaUrl", ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null)))
        .addObject("scapReference", scap.getReference())
        .addObject("feedbackUrl",
            ReverseRouter.route(on(FeedbackController.class).renderFeedbackForm(scapId, null)));

  }

  @PostMapping
  ModelAndView submitScap(@PathVariable("scapId") ScapId scapId,
                          @ModelAttribute("form") ReviewAndSubmitForm form,
                          BindingResult bindingResult) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);

    if (!scapSubmissionService.isScapValid(scapDetail)) {
      throw new ScapBadRequestException(
          "Could not submit SCAP with ID [%d] as it is not complete".formatted(scapId.scapId()));
    }

    bindingResult = reviewAndSubmitFormService.validate(form, bindingResult);
    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        scapSubmissionConfirmationModelAndView(scapId, scapDetail),
        form,
        () -> {
          scapDetailService.submitScap(scapDetail, form);
          scapEmailService.sendScapSubmissionEmails(scapDetail);
          caseEventService.recordNewEvent(SCAP_SUBMITTED, scapDetail, scapDetail.getVersionNumber(), null);

          return ReverseRouter.redirect(on(ScapSubmissionController.class).renderScapSubmissionSuccess(scapId));
        }
    );
  }

  private ModelAndView scapSubmissionConfirmationModelAndView(ScapId scapId, ScapDetail scapDetail) {
    var isValid = scapSubmissionService.isScapValid(scapDetail);
    return new ModelAndView("scap/scap/submit/reviewAndSubmit")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class)
            .renderTaskList(scapId)))
        .addObject("isValid", isValid)
        .addObject("incompleteErrorMessage", isValid ? null : INVALID_SCAP_ERROR_MESSAGE)
        .addObject("radioItems", YesNo.getRadioOptions())
        .addObject("scapSummaryView", scapSummaryViewService.getScapSummaryView(scapDetail));
  }
}
