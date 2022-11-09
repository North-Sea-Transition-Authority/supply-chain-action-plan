package uk.co.nstauthority.scap.scap.plannedtender.activity;

import java.math.BigDecimal;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.scap.RemunerationModel;

@Service
public class PlannedTenderActivityFormValidator implements Validator {
  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return PlannedTenderActivityForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (PlannedTenderActivityForm) target;

    StringInputValidator.builder().validate(form.getScopeDescription(), errors);

    DecimalInputValidator.builder()
        .mustHaveNoMoreThanDecimalPlaces(3)
        .mustBeMoreThanOrEqual(BigDecimal.valueOf(0.001))
        .validate(form.getEstimatedValue(), errors);

    ValidationUtils.rejectIfEmpty(
        errors, "remunerationModel",
        "remunerationModel.required",
        "Enter a remuneration model");

    if (RemunerationModel.OTHER.equals(form.getRemunerationModel())) {
      StringInputValidator.builder().validate(form.getRemunerationModelName(), errors);
    }

    StringInputValidator.builder().validate(form.getAwardRationale(), errors);
  }
}
