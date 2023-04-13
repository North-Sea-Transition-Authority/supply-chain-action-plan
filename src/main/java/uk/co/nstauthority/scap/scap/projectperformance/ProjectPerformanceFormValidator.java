package uk.co.nstauthority.scap.scap.projectperformance;

import com.google.common.annotations.VisibleForTesting;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDate;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.date.ThreeFieldDateInputValidator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;

@Service
class ProjectPerformanceFormValidator implements Validator {

  @VisibleForTesting
  static final String PROJECT_COMPLETED_FIELD_NAME = "projectCompleted";

  private final Clock clock;

  @Autowired
  ProjectPerformanceFormValidator(Clock clock) {
    this.clock = clock;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ProjectPerformanceForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (ProjectPerformanceForm) target;

    ValidationUtils.rejectIfEmpty(
        errors,
        PROJECT_COMPLETED_FIELD_NAME,
        "%s.required".formatted(PROJECT_COMPLETED_FIELD_NAME),
        "Select whether the full project has been completed"
    );

    if (Boolean.TRUE.equals(form.getProjectCompleted())) {

      var currentDate = LocalDate.ofInstant(clock.instant(), clock.getZone());
      currentDate = currentDate.plusDays(1);

      var startDateValidator = ThreeFieldDateInputValidator.builder()
          .mustBeBeforeDate(currentDate);

      var endDateValidator = ThreeFieldDateInputValidator.builder()
          .mustBeBeforeDate(currentDate);
      form.getStartDate().getAsLocalDate().ifPresent(endDateValidator::mustBeAfterDate);

      var outturnCostValidator = DecimalInputValidator.builder()
          .mustBeMoreThanOrEqual(BigDecimal.valueOf(0.001))
          .mustHaveNoMoreThanDecimalPlaces(3);

      startDateValidator.validate(form.getStartDate(), errors);
      endDateValidator.validate(form.getCompletionDate(), errors);
      outturnCostValidator.validate(form.getOutturnCost(), errors);
    }
  }
}
