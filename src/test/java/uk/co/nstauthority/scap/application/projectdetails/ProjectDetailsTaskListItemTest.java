package uk.co.nstauthority.scap.application.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.overview.ScapOverviewTaskListSection;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsTaskListItemTest {

  ProjectDetailsTaskListItem projectDetailsTaskListItem;

  private Integer scapId;

  @BeforeEach
  void setup() {
    projectDetailsTaskListItem = new ProjectDetailsTaskListItem();
    scapId = 34;
  }

  @Test
  void isValid() {
    assertFalse(projectDetailsTaskListItem.isValid(scapId));
  }

  @Test
  void getItemDisplayText() {
    assertThat(projectDetailsTaskListItem.getItemDisplayText()).isEqualTo("Project details");
  }

  @Test
  void getActionUrl() {
    assertThat(projectDetailsTaskListItem.getActionUrl(scapId)).isEqualTo(
        ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(scapId, null)));
  }

  @Test
  void getTaskListSection() {
    assertThat(projectDetailsTaskListItem.getTaskListSection()).isEqualTo(ScapOverviewTaskListSection.class);
  }

  @Test
  void isVisible() {
    assertTrue(projectDetailsTaskListItem.isVisible(scapId));
  }

  @Test
  void getDisplayOrder() {
    assertThat(projectDetailsTaskListItem.getDisplayOrder()).isEqualTo(20);
  }
}
