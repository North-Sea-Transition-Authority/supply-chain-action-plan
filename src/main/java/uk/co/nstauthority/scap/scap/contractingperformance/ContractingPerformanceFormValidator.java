package uk.co.nstauthority.scap.scap.contractingperformance;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Objects;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import org.springframework.validation.ValidationUtils;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
class ContractingPerformanceFormValidator implements SmartValidator {

  private static final String ACTUAL_TENDER_ACTIVITY_FIELD_NAME = "actualTenderActivityId";

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ContractingPerformanceForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    throw new IllegalStateException("Cannot validate without ContractingPerformanceFormValidatorHint");
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors, @NotNull Object... validationHints) {
    var contractingPerformanceFormValidatorHint = Arrays.stream(validationHints)
        .filter(Objects::nonNull)
        .filter(validationHint -> ContractingPerformanceFormValidatorHint.class.equals(validationHint.getClass()))
        .map(ContractingPerformanceFormValidatorHint.class::cast)
        .findFirst()
        .orElseThrow(() -> new IllegalStateException("Cannot get ContractingPerformanceFormValidatorHint"));

    var form = (ContractingPerformanceForm) target;

    ValidationUtils.rejectIfEmpty(
        errors,
        ACTUAL_TENDER_ACTIVITY_FIELD_NAME,
        "%s.required".formatted(ACTUAL_TENDER_ACTIVITY_FIELD_NAME),
        "Select the scope title this contract performance is for");
    if (!errors.hasFieldErrors(ACTUAL_TENDER_ACTIVITY_FIELD_NAME)
        && !contractingPerformanceFormValidatorHint.activityIds().contains(form.getActualTenderActivityId())) {
      errors.rejectValue(
          ACTUAL_TENDER_ACTIVITY_FIELD_NAME,
          "%s.doesNotExist".formatted(ACTUAL_TENDER_ACTIVITY_FIELD_NAME),
          "Select an actual tender activity which has a contract awarded"
      );
    }

    DecimalInputValidator.builder()
        .mustBeMoreThanOrEqual(BigDecimal.valueOf(0.001))
        .mustHaveNoMoreThanDecimalPlaces(3)
        .validate(form.getOutturnCost(), errors);

    StringInputValidator.builder()
        .isOptional()
        .validate(form.getOutturnRationale(), errors);
  }
}
