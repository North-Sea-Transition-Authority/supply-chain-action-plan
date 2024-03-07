package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@Service
class HasMoreContractingPerformanceFormValidator implements Validator {

  static final String HAS_MORE_CONTRACTING_PERFORMANCE_FIELD_NAME = "hasMoreContractingPerformance";

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return HasMoreContractingPerformanceForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    ValidationUtils.rejectIfEmpty(errors,
        HAS_MORE_CONTRACTING_PERFORMANCE_FIELD_NAME,
        "%s.required".formatted(HAS_MORE_CONTRACTING_PERFORMANCE_FIELD_NAME),
        "Select whether you have more contracting performance to add");
  }
}
