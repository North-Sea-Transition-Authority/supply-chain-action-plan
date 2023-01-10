package uk.co.nstauthority.scap.scap.projectperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
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
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class ProjectPerformanceTaskListItemTest {

  @Mock
  ScapService scapService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  ProjectPerformanceService projectPerformanceService;

  @Mock
  ProjectPerformanceFormService projectPerformanceFormService;

  @InjectMocks
  ProjectPerformanceTaskListItem projectPerformanceTaskListItem;

  @Test
  void getItemDisplayText() {
    assertThat(projectPerformanceTaskListItem.getItemDisplayText())
        .isEqualTo(ProjectPerformanceTaskListItem.DISPLAY_TEXT);
  }

  @Test
  void getActionUrl() {
    var scapId = new ScapId(48);
    var expectedActionUrl = ReverseRouter.route(on(ProjectPerformanceController.class)
        .renderProjectPerformanceForm(scapId));

    assertThat(projectPerformanceTaskListItem.getActionUrl(scapId.scapId())).isEqualTo(expectedActionUrl);
  }

  @Test
  void getDisplayOrder() {
    assertThat(projectPerformanceTaskListItem.getDisplayOrder()).isEqualTo(60);
  }

  @Test
  void isVisible() {
    assertTrue(projectPerformanceTaskListItem.isVisible(0));
  }

  @Test
  void isValid_NoProjectPerformance_AssertFalse() {
    var scapId = 48;
    var scap = new Scap(scapId);
    var scapDetail = new ScapDetail();

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertFalse(projectPerformanceTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_InvalidProjectPerformance_AssertFalse() {
    var scapId = 48;
    var scap = new Scap(scapId);
    var scapDetail = new ScapDetail();
    var projectPerformance = new ProjectPerformance();
    var form = new ProjectPerformanceForm();
    var bindingResultWithErrors = new BeanPropertyBindingResult(form, "form");
    bindingResultWithErrors.addError(new FieldError(
        "form", "testField", "test error message"
    ));

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectPerformance));
    when(projectPerformanceFormService.getForm(projectPerformance)).thenReturn(form);
    when(projectPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithErrors);

    assertFalse(projectPerformanceTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_ValidProjectPerformance_AssertTrue() {
    var scapId = 48;
    var scap = new Scap(scapId);
    var scapDetail = new ScapDetail();
    var projectPerformance = new ProjectPerformance();
    var form = new ProjectPerformanceForm();
    var bindingResultWithoutErrors = new BeanPropertyBindingResult(form, "form");

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(projectPerformanceService.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectPerformance));
    when(projectPerformanceFormService.getForm(projectPerformance)).thenReturn(form);
    when(projectPerformanceFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(bindingResultWithoutErrors);

    assertTrue(projectPerformanceTaskListItem.isValid(scapId));
  }

  @Test
  void getTaskListSection() {
    assertThat(projectPerformanceTaskListItem.getTaskListSection())
        .isEqualTo(ScapFormTaskListSection.class);
  }
}
