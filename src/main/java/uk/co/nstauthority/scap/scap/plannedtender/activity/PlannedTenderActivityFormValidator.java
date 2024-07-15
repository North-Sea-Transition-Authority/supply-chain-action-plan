package uk.co.nstauthority.scap.scap.plannedtender.activity;

import jakarta.validation.constraints.NotNull;
import java.math.BigDecimal;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.date.ThreeFieldDateInputValidator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.util.ValidationUtil;

@Service
public class PlannedTenderActivityFormValidator implements Validator {
  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return PlannedTenderActivityForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (PlannedTenderActivityForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getScopeDescription(), errors);

    DecimalInputValidator.builder()
        .mustHaveNoMoreThanDecimalPlaces(3)
        .mustBeMoreThanOrEqualTo(BigDecimal.valueOf(0.001))
        .validate(form.getEstimatedValue(), errors);

    ValidationUtils.rejectIfEmpty(
        errors, "remunerationModel",
        "remunerationModel.required",
        "Enter a remuneration model");

    if (RemunerationModel.OTHER.equals(form.getRemunerationModel())) {
      StringInputValidator.builder()
          .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
          .validate(form.getRemunerationModelName(), errors);
    }

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getAwardRationale(), errors);

    ThreeFieldDateInputValidator.builder().validate(form.getIndicativeActualTenderStartDate(), errors);

    var expectedTenderValidator = ThreeFieldDateInputValidator.builder();
    var expectedTenderStartDateOpt = form.getIndicativeActualTenderStartDate().getAsLocalDate();
    if (expectedTenderStartDateOpt.isPresent()) {
      expectedTenderValidator = expectedTenderValidator.mustBeAfterDate(expectedTenderStartDateOpt.get());
    }
    expectedTenderValidator.validate(form.getIndicativeContractAwardDate(), errors);
  }
}
