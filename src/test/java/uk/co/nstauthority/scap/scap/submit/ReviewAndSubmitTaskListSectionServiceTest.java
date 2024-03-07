package uk.co.nstauthority.scap.scap.submit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.tasklist.TaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@ExtendWith(MockitoExtension.class)
class ReviewAndSubmitTaskListSectionServiceTest {

  @InjectMocks
  private ReviewAndSubmitTaskListSectionService reviewAndSubmitTaskListSectionService;

  private static final ScapId SCAP_ID = new ScapId(242);
  private static final Scap SCAP = ScapEntityTestUtil.scapBuilder()
      .withScapId(SCAP_ID)
      .build();
  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder()
      .withScap(SCAP)
      .build();

  @Test
  void getSection() {
    var submitUrl = ReverseRouter.route(on(ScapSubmissionController.class).renderScapSubmissionConfirmation(SCAP_ID));
    var taskListSection = reviewAndSubmitTaskListSectionService.getSection(SCAP_DETAIL);

    assertThat(taskListSection).isNotEmpty();
    assertThat(taskListSection.get()).extracting(
        TaskListSection::displayName,
        TaskListSection::displayOrder
    ).containsExactly(
        ReviewAndSubmitTaskListSectionService.DISPLAY_NAME,
        ReviewAndSubmitTaskListSectionService.DISPLAY_ORDER
    );
    assertThat(taskListSection.get().items()).extracting(
        TaskListItem::displayName,
        TaskListItem::actionUrl,
        TaskListItem::label,
        TaskListItem::labelHint
    ).containsExactly(
        tuple(
            ReviewAndSubmitTaskListSectionService.DISPLAY_NAME,
            submitUrl,
            null,
            null
        )
    );
  }
}