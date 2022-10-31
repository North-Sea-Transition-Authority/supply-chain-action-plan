package uk.co.nstauthority.scap.application.projectdetails;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.application.overview.ScapOverviewTaskListSection;
import uk.co.nstauthority.scap.application.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
class ProjectDetailsTaskListItem implements ScapTaskListItem {
  private static final String DISPLAY_NAME = "Project details";
  private static final Integer DISPLAY_ORDER = 20;

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(target, null));
  }

  @Override
  public int getDisplayOrder() {
    return DISPLAY_ORDER;
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
