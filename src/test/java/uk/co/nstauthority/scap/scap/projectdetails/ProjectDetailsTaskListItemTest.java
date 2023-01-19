package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Collections;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsTaskListItemTest {

  @Mock
  ScapService scapService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  ProjectDetailsService projectDetailsService;

  @Mock
  ProjectDetailsFormService projectDetailsFormService;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ProjectDetailsTaskListItem projectDetailsTaskListItem;

  private Integer scapId;
  private Scap scap;
  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapId = 34;
    scap = new Scap(scapId);
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, clock.instant(), 1);
  }

  @Test
  void isValid_noProjectDetails_assertFalse() {
    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(projectDetailsService.getProjectDetails(scapDetail)).thenReturn(Optional.empty());

    assertFalse(projectDetailsTaskListItem.isValid(scap.getId()));
  }

  @Test
  void isValid_invalidProjectDetails_assertFalse() {
    var projectDetails = new ProjectDetails(scapDetail, clock.instant());
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("form", "testField", "Test field must not be blank"));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(projectDetailsService.getProjectDetails(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsFormService.getForm(projectDetails, Collections.emptySet(), Collections.emptySet())).thenReturn(form);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    assertFalse(projectDetailsTaskListItem.isValid(scap.getId()));
  }

  @Test
  void isValid_validProjectDetails_assertTrue() {
    var projectDetails = new ProjectDetails(scapDetail, clock.instant());
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(projectDetailsService.getProjectDetails(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsFormService.getForm(projectDetails, Collections.emptySet(), Collections.emptySet())).thenReturn(form);
    when(projectDetailsFormService.validate(eq(form), any(BindingResult.class))).thenReturn(bindingResult);

    assertTrue(projectDetailsTaskListItem.isValid(scap.getId()));
  }

  @Test
  void getItemDisplayText() {
    assertThat(projectDetailsTaskListItem.getItemDisplayText()).isEqualTo("Project details");
  }

  @Test
  void getActionUrl() {
    assertThat(projectDetailsTaskListItem.getActionUrl(scapId)).isEqualTo(
        ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(new ScapId(scapId))));
  }

  @Test
  void getTaskListSection() {
    assertThat(projectDetailsTaskListItem.getTaskListSection()).isEqualTo(ScapFormTaskListSection.class);
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
