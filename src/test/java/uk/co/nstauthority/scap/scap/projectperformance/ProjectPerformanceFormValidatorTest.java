package uk.co.nstauthority.scap.scap.projectperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ProjectPerformanceFormValidatorTest {

  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  ProjectPerformanceFormValidator projectPerformanceFormValidator;

  private ProjectPerformanceForm form;
  private BindingResult errors;

  @BeforeEach
  void setup() {
    projectPerformanceFormValidator = new ProjectPerformanceFormValidator(clock);
    form = new ProjectPerformanceForm();
    errors = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_ProjectPerformanceForm_AssertTrue() {
    assertTrue(projectPerformanceFormValidator.supports(ProjectPerformanceForm.class));
  }

  @Test
  void supports_NonSupportedForm_AssertFalse() {
    assertFalse(projectPerformanceFormValidator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_EmptyForm_AssertErrors() {
    projectPerformanceFormValidator.validate(form, errors);

    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);
    var expectedErrorField = ProjectPerformanceFormValidator.PROJECT_COMPLETED_FIELD_NAME;

    assertThat(extractedErrors).containsExactly(
        entry(expectedErrorField, Set.of("%s.required".formatted(expectedErrorField)))
    );
  }

  @Test
  void validate_ProjectNotCompleted_AssertValid() {
    form.setProjectCompleted(false);

    projectPerformanceFormValidator.validate(form, errors);

    assertFalse(errors.hasErrors());
  }

  @Test
  void validate_ProjectCompleted_OtherwiseEmpty_AssertErrors() {
    form.setProjectCompleted(true);

    projectPerformanceFormValidator.validate(form, errors);

    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("startDay.inputValue", Set.of("startDay.required")),
        entry("startMonth.inputValue", Set.of("startMonth.required")),
        entry("startYear.inputValue", Set.of("startYear.required")),
        entry("completionDay.inputValue", Set.of("completionDay.required")),
        entry("completionMonth.inputValue", Set.of("completionMonth.required")),
        entry("completionYear.inputValue", Set.of("completionYear.required")),
        entry("outturnCost.inputValue", Set.of("outturnCost.required"))
    );
  }

  @Test
  void validate_FutureDates_AssertErrors() {
    var currentInstant = clock.instant();
    var currentDate = LocalDate.ofInstant(currentInstant, clock.getZone());
    form.setProjectCompleted(true);
    form.setStartDay(String.valueOf(currentDate.getDayOfMonth() + 1));
    form.setStartMonth(String.valueOf(currentDate.getMonthValue()));
    form.setStartYear(String.valueOf(currentDate.getYear()));
    form.setCompletionDay(String.valueOf(currentDate.getDayOfMonth() + 1));
    form.setCompletionMonth(String.valueOf(currentDate.getMonthValue()));
    form.setCompletionYear(String.valueOf(currentDate.getYear() + 1));
    form.setOutturnCost("1");

    projectPerformanceFormValidator.validate(form, errors);

    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("startDay.inputValue", Set.of("startDay.maxDateExclusiveExceeded")),
        entry("startMonth.inputValue", Set.of("startMonth.maxDateExclusiveExceeded")),
        entry("startYear.inputValue", Set.of("startYear.maxDateExclusiveExceeded")),
        entry("completionDay.inputValue", Set.of("completionDay.maxDateExclusiveExceeded")),
        entry("completionMonth.inputValue", Set.of("completionMonth.maxDateExclusiveExceeded")),
        entry("completionYear.inputValue", Set.of("completionYear.maxDateExclusiveExceeded"))
    );
  }

  @Test
  void validate_EndBeforeStart_AssertErrors() {
    var currentInstant = clock.instant();
    var currentDate = LocalDate.ofInstant(currentInstant, clock.getZone());
    form.setProjectCompleted(true);
    form.setStartDay(String.valueOf(currentDate.getDayOfMonth() - 1));
    form.setStartMonth(String.valueOf(currentDate.getMonthValue()));
    form.setStartYear(String.valueOf(currentDate.getYear()));
    form.setCompletionDay(String.valueOf(currentDate.getDayOfMonth() - 1));
    form.setCompletionMonth(String.valueOf(currentDate.getMonthValue()));
    form.setCompletionYear(String.valueOf(currentDate.getYear() - 1));
    form.setOutturnCost("1");

    projectPerformanceFormValidator.validate(form, errors);

    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("completionDay.inputValue", Set.of("completionDay.minDateExclusiveNotMet")),
        entry("completionMonth.inputValue", Set.of("completionMonth.minDateExclusiveNotMet")),
        entry("completionYear.inputValue", Set.of("completionYear.minDateExclusiveNotMet"))
    );
  }

  @Test
  void validate_ProjectCompleted_AssertValid() {
    var currentInstant = clock.instant();
    var currentDate = LocalDate.ofInstant(currentInstant, clock.getZone());
    form.setProjectCompleted(true);
    form.setStartDay(String.valueOf(currentDate.getDayOfMonth() - 1));
    form.setStartMonth(String.valueOf(currentDate.getMonthValue()));
    form.setStartYear(String.valueOf(currentDate.getYear() - 1));
    form.setCompletionDay(String.valueOf(currentDate.getDayOfMonth() - 1));
    form.setCompletionMonth(String.valueOf(currentDate.getMonthValue()));
    form.setCompletionYear(String.valueOf(currentDate.getYear()));
    form.setOutturnCost("1");

    projectPerformanceFormValidator.validate(form, errors);

    assertFalse(errors.hasErrors());
  }
}
