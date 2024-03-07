package uk.co.nstauthority.scap.scap.submit;

import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;

@Service
class ReviewAndSubmitFormValidator implements Validator {

  static final String APPROVED_BY_STAKEHOLDERS_FIELD = "approvedByStakeholders";
  static final String MUST_BE_APPROVED_ERROR_MESSAGE =
      "Confirm that this SCAP has been checked, reviewed and approved by all of your internal stakeholders";

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ReviewAndSubmitForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    ValidationUtils.rejectIfEmpty(
        errors,
        APPROVED_BY_STAKEHOLDERS_FIELD,
        "%s.required".formatted(APPROVED_BY_STAKEHOLDERS_FIELD),
        MUST_BE_APPROVED_ERROR_MESSAGE
    );
  }
}
