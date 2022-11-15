package uk.co.nstauthority.scap.scap.actualtender.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/actual-tender/summary")
public class ActualTenderSummaryController {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderSummaryService actualTenderSummaryService;

  ActualTenderSummaryController(ScapService scapService, ScapDetailService scapDetailService,
                                ActualTenderService actualTenderService,
                                ActualTenderActivityService actualTenderActivityService,
                                ActualTenderSummaryService actualTenderSummaryService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderSummaryService = actualTenderSummaryService;
  }

  @GetMapping
  public ModelAndView renderActualTenderSummary(@PathVariable("scapId") Integer scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetailOrThrow(scapDetail);
    var actualTenderActivities = actualTenderActivityService.getAllByActualTender(actualTender);

    if (actualTenderActivities.isEmpty()) {
      return ReverseRouter.redirect(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));
    }

    var actualTenderSummaryViews = actualTenderSummaryService
        .getViewsForActualTenderActivities(actualTenderActivities, scapId);

    return actualTenderSummaryModelAndView(scapId, actualTenderSummaryViews);
  }

  private ModelAndView actualTenderSummaryModelAndView(Integer scapId, List<ActualTenderSummaryView> summaryViews) {
    return new ModelAndView("scap/scap/actualtender/actualTenderActivitySummary")
        .addObject("actualTenderActivities", summaryViews)
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)));
  }
}
