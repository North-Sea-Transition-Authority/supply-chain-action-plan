package uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@Service
public class HasPlannedTenderFormValidator implements Validator {
  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return HasPlannedTenderForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    ValidationUtils.rejectIfEmptyOrWhitespace(errors,
        "hasPlannedTender",
        "hasPlannedTender.presence",
        "Select whether this SCAP has planned tender activities");
  }
}
