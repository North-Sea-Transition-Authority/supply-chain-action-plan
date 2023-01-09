package uk.co.nstauthority.scap.scap.plannedtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class PlannedTenderTaskListItem implements ScapTaskListItem {

  private static final String DISPLAY_NAME = "Planned tender activity";
  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final PlannedTenderService plannedTenderService;

  @Autowired
  public PlannedTenderTaskListItem(ScapService scapService, ScapDetailService scapDetailService,
                                   PlannedTenderService plannedTenderService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.plannedTenderService = plannedTenderService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(HasPlannedTenderController.class).renderHasPlannedTenderActivityForm(new ScapId(target)));
  }

  @Override
  public int getDisplayOrder() {
    return 30;
  }

  @Override
  public boolean isValid(Integer target) {
    var scap = scapService.getScapById(target);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var scapPlannedTender = plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);

    return scapPlannedTender.map(plannedTender -> {
      if (Boolean.TRUE.equals(plannedTender.getHasPlannedTenders())) {
        return HasMorePlannedTenderActivities.NO.equals(plannedTender.getHasMorePlannedTenderActivities());
      }
      return Boolean.FALSE.equals(plannedTender.getHasPlannedTenders());
    }).orElse(false);
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
