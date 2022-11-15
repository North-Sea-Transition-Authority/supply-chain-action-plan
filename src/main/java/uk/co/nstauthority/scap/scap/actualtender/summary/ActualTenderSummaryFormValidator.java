package uk.co.nstauthority.scap.scap.actualtender.summary;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@Service
class ActualTenderSummaryFormValidator implements Validator {
  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ActualTenderSummaryForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    ValidationUtils.rejectIfEmpty(errors,
        "hasMoreActualTenderActivities",
        "hasMoreActualTenderActivities.required",
        "Select whether you have more actual tender activities to add"
    );
  }
}
