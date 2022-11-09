package uk.co.nstauthority.scap.scap.actualtender.hasactualtender;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@Service
class HasActualTenderFormValidator implements Validator {
  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return HasActualTenderForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    ValidationUtils.rejectIfEmpty(
        errors,
        "hasActualTender",
        "hasActualTender.required",
        "Select whether this SCAP has actual planned tendering activity"
    );
  }
}
