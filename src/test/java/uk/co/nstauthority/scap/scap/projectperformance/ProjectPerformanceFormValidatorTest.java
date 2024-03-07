package uk.co.nstauthority.scap.scap.projectperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
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
        entry("startDate.dayInput.inputValue", Set.of("startDate.dayInput.required")),
        entry("startDate.monthInput.inputValue", Set.of("startDate.monthInput.required")),
        entry("startDate.yearInput.inputValue", Set.of("startDate.yearInput.required")),
        entry("completionDate.dayInput.inputValue", Set.of("completionDate.dayInput.required")),
        entry("completionDate.monthInput.inputValue", Set.of("completionDate.monthInput.required")),
        entry("completionDate.yearInput.inputValue", Set.of("completionDate.yearInput.required")),
        entry("outturnCost.inputValue", Set.of("outturnCost.required"))
    );
  }

  @Test
  void validate_FutureDates_AssertErrors() {
    var currentInstant = clock.instant();
    var currentDate = LocalDate.ofInstant(currentInstant, clock.getZone());
    form.setProjectCompleted(true);
    form.setStartDate(currentDate.plus(1, ChronoUnit.DAYS));
    form.setCompletionDate(currentDate.plus(2, ChronoUnit.DAYS));
    form.setOutturnCost("1");

    projectPerformanceFormValidator.validate(form, errors);

    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("startDate.dayInput.inputValue", Set.of("startDate.dayInput.maxDateExclusiveExceeded")),
        entry("startDate.monthInput.inputValue", Set.of("startDate.monthInput.maxDateExclusiveExceeded")),
        entry("startDate.yearInput.inputValue", Set.of("startDate.yearInput.maxDateExclusiveExceeded")),
        entry("completionDate.dayInput.inputValue", Set.of("completionDate.dayInput.maxDateExclusiveExceeded")),
        entry("completionDate.monthInput.inputValue", Set.of("completionDate.monthInput.maxDateExclusiveExceeded")),
        entry("completionDate.yearInput.inputValue", Set.of("completionDate.yearInput.maxDateExclusiveExceeded"))
    );
  }

  @Test
  void validate_EndBeforeStart_AssertErrors() {
    var currentInstant = clock.instant();
    var currentDate = LocalDate.ofInstant(currentInstant, clock.getZone());
    form.setProjectCompleted(true);
    form.setStartDate(currentDate.minus(1, ChronoUnit.MONTHS));
    form.setCompletionDate(currentDate.minus(1, ChronoUnit.YEARS));
    form.setOutturnCost("1");

    projectPerformanceFormValidator.validate(form, errors);

    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("completionDate.dayInput.inputValue", Set.of("completionDate.dayInput.minDateExclusiveNotMet")),
        entry("completionDate.monthInput.inputValue", Set.of("completionDate.monthInput.minDateExclusiveNotMet")),
        entry("completionDate.yearInput.inputValue", Set.of("completionDate.yearInput.minDateExclusiveNotMet"))
    );
  }

  @Test
  void validate_ProjectCompleted_AssertValid() {
    var currentInstant = clock.instant();
    var currentDate = LocalDate.ofInstant(currentInstant, clock.getZone());
    form.setProjectCompleted(true);
    form.setStartDate(currentDate.minus(1, ChronoUnit.YEARS));
    form.setCompletionDate(currentDate.minus(1, ChronoUnit.DAYS));
    form.setOutturnCost("1");

    projectPerformanceFormValidator.validate(form, errors);

    assertFalse(errors.hasErrors());
  }
}
