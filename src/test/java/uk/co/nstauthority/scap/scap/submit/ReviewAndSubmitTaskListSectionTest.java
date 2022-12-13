package uk.co.nstauthority.scap.scap.submit;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ReviewAndSubmitTaskListSectionTest {

  @InjectMocks
  ReviewAndSubmitTaskListSection reviewAndSubmitTaskListSection;

  @Test
  void getSectionName() {
    assertThat(reviewAndSubmitTaskListSection.getSectionName()).isEqualTo(ReviewAndSubmitTaskListSection.SECTION_NAME);
  }

  @Test
  void getDisplayOrder() {
    assertThat(reviewAndSubmitTaskListSection.getDisplayOrder()).isEqualTo(20);
  }
}
