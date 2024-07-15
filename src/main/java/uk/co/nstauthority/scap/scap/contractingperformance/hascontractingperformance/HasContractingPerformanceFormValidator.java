package uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance;

import jakarta.validation.constraints.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@Service
class HasContractingPerformanceFormValidator implements Validator {
  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return HasContractingPerformanceForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    ValidationUtils.rejectIfEmpty(
        errors,
        "hasContractingPerformance",
        "hasContractingPerformance.required",
        "Select whether any of the awarded contracts been fully closed out"
    );
  }
}
