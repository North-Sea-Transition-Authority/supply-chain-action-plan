package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import com.google.common.annotations.VisibleForTesting;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.HasMoreContractingPerformance;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class ContractingPerformanceTaskListItem implements ScapTaskListItem {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @VisibleForTesting
  static final String DISPLAY_TEXT = "Contracting performance";

  public ContractingPerformanceTaskListItem(ScapService scapService, ScapDetailService scapDetailService,
                                            ContractingPerformanceOverviewService contractingPerformanceOverviewService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_TEXT;
  }

  @Override
  public String getActionUrl(Integer scapId) {
    return ReverseRouter.route(on(HasContractingPerformanceController.class).renderHasContractingPerformanceForm(scapId));
  }

  @Override
  public int getDisplayOrder() {
    return 50;
  }

  @Override
  public boolean isValid(Integer target) {
    var scap = scapService.getScapById(target);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var contractingPerformanceOverview = contractingPerformanceOverviewService.getByScapDetail(scapDetail);

    return contractingPerformanceOverview
        .map(existingContractingPerformanceOverview -> {
          if (Boolean.FALSE.equals(existingContractingPerformanceOverview.getHasContractingPerformance())) {
            return true;
          }
          return HasMoreContractingPerformance.NO
              .equals(existingContractingPerformanceOverview.getHasMoreContractingPerformance());
        })
        .orElse(false);
  }

  @Override
  public boolean isVisible(Integer target) {
    return true;
  }

  @Override
  public Class<? extends TaskListSection<Integer>> getTaskListSection() {
    return ScapFormTaskListSection.class;
  }
}
