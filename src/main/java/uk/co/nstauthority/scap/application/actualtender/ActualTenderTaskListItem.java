package uk.co.nstauthority.scap.application.actualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.application.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.application.overview.ScapOverviewTaskListSection;
import uk.co.nstauthority.scap.application.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class ActualTenderTaskListItem implements ScapTaskListItem {

  private static final String DISPLAY_NAME = "Actual tendering activity";

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(target));
  }

  @Override
  public int getDisplayOrder() {
    return 40;
  }

  @Override
  public boolean isValid(Integer target) {
    return false;
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
