package uk.co.nstauthority.scap.scap.submit;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class ReviewAndSubmitTaskListItem implements ScapTaskListItem {

  static final String DISPLAY_TEXT = "Review and submit";

  @Override
  public String getItemDisplayText() {
    return DISPLAY_TEXT;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionConfirmation(new ScapId(target)));
  }

  @Override
  public int getDisplayOrder() {
    return 10;
  }

  @Override
  public boolean isVisible(Integer target) {
    return true;
  }

  @Override
  public Class<? extends TaskListSection<Integer>> getTaskListSection() {
    return ReviewAndSubmitTaskListSection.class;
  }

  @Override
  public boolean showNotCompletedLabels(Integer target) {
    return false;
  }
}
