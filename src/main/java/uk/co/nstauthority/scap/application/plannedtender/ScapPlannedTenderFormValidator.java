package uk.co.nstauthority.scap.application.plannedtender;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@Service
class ScapPlannedTenderFormValidator implements Validator {
  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ScapPlannedTenderForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    ValidationUtils.rejectIfEmpty(
        errors,
        "hasMorePlannedTenderActivities",
        "hasMorePlannedTenderActivities.required",
        "Select whether you need to add another planned tender activity");
  }
}
