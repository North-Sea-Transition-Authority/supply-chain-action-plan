package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import com.google.common.annotations.VisibleForTesting;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class ContractingPerformanceTaskListItem implements ScapTaskListItem {

  @VisibleForTesting
  static final String DISPLAY_TEXT = "Contracting performance";

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
    return false;
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
