package uk.co.nstauthority.scap.scap.submit;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.error.exception.ScapBadRequestException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("{scapId}/submit")
class ScapSubmissionController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;

  @Autowired
  ScapSubmissionController(ScapService scapService, ScapDetailService scapDetailService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
  }

  @GetMapping
  ModelAndView renderScapSubmissionConfirmation(@PathVariable("scapId") Integer scapId) {
    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);

    if (!ScapDetailStatus.DRAFT.equals(scapDetail.getStatus())) {
      throw new ScapBadRequestException("SCAP with ID [%d] is not in DRAFT status");
    }

    return scapSubmissionConfirmationModelAndView(scapId);
  }

  @GetMapping("/success")
  ModelAndView renderScapSubmissionSuccess(@PathVariable("scapId") Integer scapId) {
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
  ModelAndView submitScap(@PathVariable("scapId") Integer scapId) {
    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);

    if (!ScapDetailStatus.DRAFT.equals(scapDetail.getStatus())) {
      throw new ScapBadRequestException("SCAP with ID [%d] is not in DRAFT status");
    }

    scapDetailService.submitScap(scapDetail);

    return ReverseRouter.redirect(on(ScapSubmissionController.class).renderScapSubmissionSuccess(scapId));
  }

  private ModelAndView scapSubmissionConfirmationModelAndView(Integer scapId) {
    return new ModelAndView("scap/scap/submit/reviewAndSubmit")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class)
            .renderTaskList(scapId)));
  }
}
