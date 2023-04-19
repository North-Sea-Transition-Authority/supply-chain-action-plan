package uk.co.nstauthority.scap.scap.tasklist;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.tasklist.TaskListSection;
import uk.co.nstauthority.scap.tasklist.TaskListSectionService;

@ExtendWith(MockitoExtension.class)
class TaskListServiceTest {

  @Mock
  private ScapService scapService;

  @Mock
  private ScapDetailService scapDetailService;

  @Mock
  private TaskListSectionService taskListSectionService1;

  private static final TaskListSection TASK_LIST_SECTION_1 = new TaskListSection(null, 0, null);

  @Mock
  private TaskListSectionService taskListSectionService2;

  private static final TaskListSection TASK_LIST_SECTION_2 = new TaskListSection(null, 0, null);

  private TaskListService taskListService;

  private static final ScapId SCAP_ID = new ScapId(23);
  private static final Scap SCAP = ScapEntityTestUtil.scapBuilder()
      .withScapId(SCAP_ID)
      .build();
  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder()
      .withScap(SCAP)
      .build();

  @BeforeEach
  void setup() {
    taskListService = new TaskListService(
        scapService,
        scapDetailService,
        List.of(taskListSectionService1, taskListSectionService2)
    );
  }

  @Test
  void getTaskListSections() {
    when(scapService.getScapById(SCAP_ID)).thenReturn(SCAP);
    when(scapDetailService.getLatestByScap(SCAP)).thenReturn(SCAP_DETAIL);
    when(taskListSectionService1.getSection(SCAP_DETAIL)).thenReturn(Optional.of(TASK_LIST_SECTION_1));
    when(taskListSectionService2.getSection(SCAP_DETAIL)).thenReturn(Optional.of(TASK_LIST_SECTION_2));

    var taskListSections = taskListService.getTaskListSections(SCAP_ID);

    assertThat(taskListSections).containsExactly(TASK_LIST_SECTION_1, TASK_LIST_SECTION_2);
  }
}
