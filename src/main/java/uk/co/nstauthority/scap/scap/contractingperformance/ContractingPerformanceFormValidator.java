package uk.co.nstauthority.scap.scap.contractingperformance;

import jakarta.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.SmartValidator;
import org.springframework.validation.ValidationUtils;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.util.ValidationUtil;

@Service
class ContractingPerformanceFormValidator implements SmartValidator {

  private final AwardedContractService awardedContractService;
  private final ActualTenderActivityService actualTenderActivityService;

  private static final String ACTUAL_TENDER_ACTIVITY_FIELD_NAME = "actualTenderActivityId";

  @Autowired
  ContractingPerformanceFormValidator(AwardedContractService awardedContractService,
                                      ActualTenderActivityService actualTenderActivityService) {
    this.awardedContractService = awardedContractService;
    this.actualTenderActivityService = actualTenderActivityService;
  }

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
        .mustBeMoreThanOrEqualTo(BigDecimal.valueOf(0.001))
        .mustHaveNoMoreThanDecimalPlaces(3)
        .validate(form.getOutturnCost(), errors);

    // If there is a valid ActualTenderActivity and a valid outrun cost
    //  and an awarded contract the outturnRationale is not optional if the awarded value is less than the outturn cost
    var isAwardedValueLessThanOutturnCost = false;
    if (!errors.hasFieldErrors(ACTUAL_TENDER_ACTIVITY_FIELD_NAME)
        || !errors.hasFieldErrors(form.getOutturnCost().getFieldName())) {
      var optionalAwardedContract = awardedContractService
          .getByActualTenderActivity(actualTenderActivityService.getById(form.getActualTenderActivityId()));

      if (optionalAwardedContract.isPresent()) {
        var awardedContract = optionalAwardedContract.get();
        var awardedValue = awardedContract.getAwardValue();
        var outtrunCost = form.getOutturnCost().getAsBigDecimal().orElse(new BigDecimal("0"));
        isAwardedValueLessThanOutturnCost = awardedValue.compareTo(outtrunCost) < 0;
      }
    }

    if (isAwardedValueLessThanOutturnCost) {
      StringInputValidator.builder()
          .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
          .validate(form.getOutturnRationale(), errors);
    } else {
      StringInputValidator.builder()
          .isOptional()
          .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
          .validate(form.getOutturnRationale(), errors);
    }
  }
}
