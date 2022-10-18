package uk.co.nstauthority.scap.application.plannedtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.plannedtender.list.PlannedTenderDetailListService;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("{scapId}/planned-tender/list")
public class ScapPlannedTenderController {

  private final ScapOverviewService scapOverviewService;
  private final ScapDetailService scapDetailService;
  private final ScapPlannedTenderService scapPlannedTenderService;
  private final ScapPlannedTenderDetailService scapPlannedTenderDetailService;
  private final PlannedTenderDetailListService plannedTenderDetailListService;

  public ScapPlannedTenderController(ScapOverviewService scapOverviewService, ScapDetailService scapDetailService,
                                     ScapPlannedTenderService scapPlannedTenderService,
                                     ScapPlannedTenderDetailService scapPlannedTenderDetailService,
                                     PlannedTenderDetailListService plannedTenderDetailListService) {
    this.scapOverviewService = scapOverviewService;
    this.scapDetailService = scapDetailService;
    this.scapPlannedTenderService = scapPlannedTenderService;
    this.scapPlannedTenderDetailService = scapPlannedTenderDetailService;
    this.plannedTenderDetailListService = plannedTenderDetailListService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderActivities(@PathVariable("scapId") Integer scapId) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var plannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);
    var existingTenderDetails = scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(plannedTender);

    return new ModelAndView("scap/application/plannedTender/list")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("plannedTenderDetailsList",
            plannedTenderDetailListService.plannedTenderDetailsToListItems(scapId, existingTenderDetails));
  }
}
