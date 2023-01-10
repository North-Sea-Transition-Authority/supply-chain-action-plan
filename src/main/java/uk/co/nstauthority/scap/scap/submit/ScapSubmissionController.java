package uk.co.nstauthority.scap.scap.submit;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.timeline.TimelineEventSubject.SCAP_SUBMITTED;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.error.exception.ScapBadRequestException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.scap.timeline.TimelineEventService;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("{scapId}/submit")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
class ScapSubmissionController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ScapSummaryViewService scapSummaryViewService;

  private final TimelineEventService timelineEventService;

  @Autowired
  ScapSubmissionController(ScapService scapService,
                           ScapDetailService scapDetailService,
                           ScapSummaryViewService scapSummaryViewService,
                           TimelineEventService timelineEventService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.timelineEventService = timelineEventService;
  }

  @GetMapping
  ModelAndView renderScapSubmissionConfirmation(@PathVariable("scapId") ScapId scapId) {
    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);

    if (!ScapDetailStatus.DRAFT.equals(scapDetail.getStatus())) {
      throw new ScapBadRequestException("SCAP with ID [%d] is not in DRAFT status");
    }

    return scapSubmissionConfirmationModelAndView(scapId)
        .addObject("scapSummaryView", scapSummaryViewService.getScapSummaryView(scapDetail));
  }

  @GetMapping("/success")
  ModelAndView renderScapSubmissionSuccess(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);

    if (!ScapDetailStatus.SUBMITTED.equals(scapDetail.getStatus())) {
      throw new ScapBadRequestException("SCAP with ID [%d] is not in SUBMITTED status");
    }

    return new ModelAndView("scap/scap/submit/submissionSuccess")
        .addObject("workAreaUrl", ReverseRouter.route(on(WorkAreaController.class).getWorkArea()))
        .addObject("scapReference", scap.getReference());
  }

  @PostMapping
  ModelAndView submitScap(@PathVariable("scapId") ScapId scapId) {
    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);

    if (!ScapDetailStatus.DRAFT.equals(scapDetail.getStatus())) {
      throw new ScapBadRequestException("SCAP with ID [%d] is not in DRAFT status");
    }

    scapDetailService.submitScap(scapDetail);
    timelineEventService.recordNewEvent(SCAP_SUBMITTED, scapId, scapDetail.getVersionNumber());

    return ReverseRouter.redirect(on(ScapSubmissionController.class).renderScapSubmissionSuccess(scapId));
  }

  private ModelAndView scapSubmissionConfirmationModelAndView(ScapId scapId) {
    return new ModelAndView("scap/scap/submit/reviewAndSubmit")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class)
            .renderTaskList(scapId)));
  }
}
