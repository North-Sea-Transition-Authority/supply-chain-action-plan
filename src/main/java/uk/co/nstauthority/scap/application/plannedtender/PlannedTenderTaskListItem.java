package uk.co.nstauthority.scap.application.plannedtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewTaskListSection;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.application.plannedtender.hasplannedtender.ScapHasPlannedTenderController;
import uk.co.nstauthority.scap.application.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class PlannedTenderTaskListItem implements ScapTaskListItem {

  private static final String DISPLAY_NAME = "Planned tender activity";
  private final ScapOverviewService scapOverviewService;
  private final ScapDetailService scapDetailService;
  private final ScapPlannedTenderService scapPlannedTenderService;
  private final ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @Autowired
  public PlannedTenderTaskListItem(ScapOverviewService scapOverviewService, ScapDetailService scapDetailService,
                                   ScapPlannedTenderService scapPlannedTenderService,
                                   ScapPlannedTenderDetailService scapPlannedTenderDetailService) {
    this.scapOverviewService = scapOverviewService;
    this.scapDetailService = scapDetailService;
    this.scapPlannedTenderService = scapPlannedTenderService;
    this.scapPlannedTenderDetailService = scapPlannedTenderDetailService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(ScapHasPlannedTenderController.class).renderHasPlannedTenderActivityForm(target));
  }

  @Override
  public int getDisplayOrder() {
    return 30;
  }

  @Override
  public boolean isValid(Integer target) {
    var scap = scapOverviewService.getScapById(target);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var scapPlannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);

    return scapPlannedTender.map(plannedTender -> {
      if (Objects.isNull(plannedTender.getHasPlannedTenders())) {
        return false;
      }
      return !plannedTender.getHasPlannedTenders() || scapPlannedTenderDetailService.hasExistingTenderDetails(plannedTender);
    })
        .orElse(false);
  }

  @Override
  public Class<? extends TaskListSection<Integer>> getTaskListSection() {
    return ScapOverviewTaskListSection.class;
  }

  @Override
  public boolean isVisible(Integer target) {
    return true;
  }
}
