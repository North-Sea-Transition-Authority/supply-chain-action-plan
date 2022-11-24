package uk.co.nstauthority.scap.scap.actualtender.activity.delete;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionService;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/actual-tender/activity/{activityId}/delete")
public class DeleteActualTenderActivityController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderSummaryViewService actualTenderSummaryViewService;
  private final DeleteActualTenderActivityService deleteActualTenderActivityService;
  private final ActualTenderControllerRedirectionService actualTenderControllerRedirectionService;

  @Autowired
  DeleteActualTenderActivityController(ScapService scapService, ScapDetailService scapDetailService,
                                       ActualTenderService actualTenderService,
                                       ActualTenderActivityService actualTenderActivityService,
                                       ActualTenderSummaryViewService actualTenderSummaryViewService,
                                       DeleteActualTenderActivityService deleteActualTenderActivityService,
                                       ActualTenderControllerRedirectionService actualTenderControllerRedirectionService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderSummaryViewService = actualTenderSummaryViewService;
    this.deleteActualTenderActivityService = deleteActualTenderActivityService;
    this.actualTenderControllerRedirectionService = actualTenderControllerRedirectionService;
  }

  @GetMapping
  public ModelAndView renderDeleteActualTenderActivityConfirmation(@PathVariable("scapId") Integer scapId,
                                                                   @PathVariable("activityId") Integer activityId) {
    scapService.getScapById(scapId);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var actualTenderActivityView = actualTenderSummaryViewService
        .getSingleViewByActualTenderActivity(actualTenderActivity, scapId);

    return new ModelAndView("scap/scap/actualtender/deleteActualTender")
        .addObject("backLinkUrl", ReverseRouter.route(on(ActualTenderSummaryController.class)
            .renderActualTenderSummary(scapId)))
        .addObject("actualTenderActivityView", actualTenderActivityView);
  }

  @PostMapping
  public ModelAndView submitDeleteActualTenderActivity(@PathVariable("scapId") Integer scapId,
                                                       @PathVariable("activityId") Integer activityId,
                                                       RedirectAttributes redirectAttributes) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    deleteActualTenderActivityService.deleteActualTenderActivity(actualTenderActivity);

    deleteActualTenderActivityService.addActualTenderDeletionSuccessBanner(
        redirectAttributes, actualTenderActivity.getScopeTitle());

    var hasRemainingActualTenderActivities = actualTenderActivityService.hasActualTenderActivity(actualTender);
    return actualTenderControllerRedirectionService.redirectFromActualTenderDeletion(scapId, hasRemainingActualTenderActivities);
  }
}
