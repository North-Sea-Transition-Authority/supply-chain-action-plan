package uk.co.nstauthority.scap.scap.submit;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.Optional;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.tasklist.TaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;
import uk.co.nstauthority.scap.tasklist.TaskListSectionService;

@Service
public class ReviewAndSubmitTaskListSectionService implements TaskListSectionService {

  static final String DISPLAY_NAME = "Review and submit";
  static final Integer DISPLAY_ORDER = 2;

  @Override
  public Optional<TaskListSection> getSection(ScapDetail scapDetail) {
    var submitUrl = ReverseRouter.route(on(ScapSubmissionController.class)
        .renderScapSubmissionConfirmation(scapDetail.getScap().getScapId()));

    return Optional.of(new TaskListSection(
        DISPLAY_NAME,
        DISPLAY_ORDER,
        Collections.singletonList(TaskListItem.withoutLabel(DISPLAY_NAME, submitUrl))
    ));
  }
}
