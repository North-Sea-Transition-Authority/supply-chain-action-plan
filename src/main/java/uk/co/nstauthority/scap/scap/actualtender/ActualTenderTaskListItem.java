package uk.co.nstauthority.scap.scap.actualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class ActualTenderTaskListItem implements ScapTaskListItem {

  private static final String DISPLAY_NAME = "Actual tender activity";

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderSummaryViewService actualTenderSummaryViewService;

  @Autowired
  public ActualTenderTaskListItem(ScapService scapService,
                                  ScapDetailService scapDetailService,
                                  ActualTenderService actualTenderService,
                                  ActualTenderActivityService actualTenderActivityService,
                                  ActualTenderSummaryViewService actualTenderSummaryViewService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderSummaryViewService = actualTenderSummaryViewService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer scapId) {
    return ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(new ScapId(scapId)));
  }

  @Override
  public int getDisplayOrder() {
    return 40;
  }

  @Override
  public boolean isValid(Integer scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTenderOpt = actualTenderService.getByScapDetail(scapDetail);

    if (actualTenderOpt.isEmpty()) {
      return false;
    }
    var actualTender = actualTenderOpt.get();

    if (Boolean.FALSE.equals(actualTender.getHasActualTenders())) {
      return true;
    }

    if (!HasMoreActualTenderActivities.NO.equals(actualTender.getHasMoreActualTenders())) {
      return false;
    }

    var actualTenderActivities = actualTenderActivityService.getAllByActualTender(actualTender);
    var actualTenderActivitySummaryViews = actualTenderSummaryViewService
        .getByActualTenderActivities(actualTenderActivities, new ScapId(scapId));
    return actualTenderActivitySummaryViews.stream()
        .allMatch(ActualTenderActivitySummaryView::isValid);

  }

  @Override
  public Class<? extends TaskListSection<Integer>> getTaskListSection() {
    return ScapFormTaskListSection.class;
  }

  @Override
  public boolean isVisible(Integer target) {
    return true;
  }
}
