package uk.co.nstauthority.scap.scap.submit;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSectionService;
import uk.co.nstauthority.scap.tasklist.TaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListLabel;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@ExtendWith(MockitoExtension.class)
class ScapSubmissionServiceTest {

  @Mock
  private ScapFormTaskListSectionService scapFormTaskListSectionService;

  @InjectMocks
  private ScapSubmissionService scapSubmissionService;

  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder().build();

  @Test
  void isScapValid_NoScapFormSection_AssertFalse() {
    when(scapFormTaskListSectionService.getSection(SCAP_DETAIL)).thenReturn(Optional.empty());

    assertFalse(scapSubmissionService.isScapValid(SCAP_DETAIL));
  }

  @Test
  void isScapValid_NoItemsComplete_AssertFalse() {
    var taskListItems = List.of(
        new TaskListItem("test item 1", TaskListLabel.NOT_COMPLETED, "#"),
        new TaskListItem("test item 2", TaskListLabel.BLOCKED, "#")
    );
    when(scapFormTaskListSectionService.getSection(SCAP_DETAIL))
        .thenReturn(Optional.of(new TaskListSection("Test section", 1, taskListItems)));

    assertFalse(scapSubmissionService.isScapValid(SCAP_DETAIL));
  }

  // Done as a parameterised test to ensure that no labels except COMPLETED are marked as valid
  @ParameterizedTest
  @EnumSource(value = TaskListLabel.class, names = "COMPLETED", mode = EnumSource.Mode.EXCLUDE)
  void isScapValid_SomeItemsComplete_AssertFalse(TaskListLabel label) {
    var taskListItems = List.of(
        new TaskListItem("test item 1", TaskListLabel.COMPLETED, "#"),
        new TaskListItem("test item 2", label, "#")
    );
    when(scapFormTaskListSectionService.getSection(SCAP_DETAIL))
        .thenReturn(Optional.of(new TaskListSection("Test section", 1, taskListItems)));

    assertFalse(scapSubmissionService.isScapValid(SCAP_DETAIL));
  }

  @Test
  void isScapValid_AllItemsComplete_AssertTrue() {
    var taskListItems = List.of(
        new TaskListItem("test item 1", TaskListLabel.COMPLETED, "#"),
        new TaskListItem("test item 2", TaskListLabel.COMPLETED, "#")
    );
    when(scapFormTaskListSectionService.getSection(SCAP_DETAIL))
        .thenReturn(Optional.of(new TaskListSection("Test section", 1, taskListItems)));

    assertTrue(scapSubmissionService.isScapValid(SCAP_DETAIL));
  }
}