package uk.co.nstauthority.scap.scap.submit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
class ReviewAndSubmitTaskListItemTest {

  @InjectMocks
  ReviewAndSubmitTaskListItem reviewAndSubmitTaskListItem;

  @Test
  void getSectionName() {
    assertThat(reviewAndSubmitTaskListItem.getItemDisplayText()).isEqualTo(ReviewAndSubmitTaskListItem.DISPLAY_TEXT);
  }

  @Test
  void getActionUrl() {
    var scapId = 56;
    var expectedUrl = ReverseRouter.route(on(ScapSubmissionController.class)
        .renderScapSubmissionConfirmation(scapId));

    assertThat(reviewAndSubmitTaskListItem.getActionUrl(scapId)).isEqualTo(expectedUrl);
  }

  @Test
  void getDisplayOrder() {
    assertThat(reviewAndSubmitTaskListItem.getDisplayOrder()).isEqualTo(10);
  }

  @Test
  void isVisible() {
    assertTrue(reviewAndSubmitTaskListItem.isVisible(0));
  }

  @Test
  void getTaskListSection() {
    assertThat(reviewAndSubmitTaskListItem.getTaskListSection())
        .isEqualTo(ReviewAndSubmitTaskListSection.class);
  }

  @Test
  void showNotCompletedLabels() {
    assertFalse(reviewAndSubmitTaskListItem.showNotCompletedLabels(0));
  }
}
