package uk.co.nstauthority.scap.scap.submit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSectionService;
import uk.co.nstauthority.scap.tasklist.TaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListLabel;

@Service
class ScapSubmissionService {

  private final ScapFormTaskListSectionService scapFormTaskListSectionService;

  @Autowired
  ScapSubmissionService(ScapFormTaskListSectionService scapFormTaskListSectionService) {
    this.scapFormTaskListSectionService = scapFormTaskListSectionService;
  }

  boolean isScapValid(ScapDetail scapDetail) {
    var scapFormSectionOpt = scapFormTaskListSectionService.getSection(scapDetail);
    if (scapFormSectionOpt.isEmpty()) {
      return false;
    }

    var taskListItems = scapFormSectionOpt.get().items();

    return taskListItems.stream()
        .map(TaskListItem::label)
        .allMatch(TaskListLabel.COMPLETED::equals);
  }
}
