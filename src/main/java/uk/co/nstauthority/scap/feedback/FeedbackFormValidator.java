package uk.co.nstauthority.scap.feedback;

import java.util.Objects;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.util.ValidationUtil;

@Service
class FeedbackFormValidator {
  public BindingResult validate(FeedbackForm form, BindingResult bindingResult) {
    if (Objects.isNull(form.getSatisfactionRating())) {
      bindingResult.rejectValue(
          FeedbackForm.SATISFACTION_FIELD_NAME,
          "required",
          "Select how satisfied you were with this service");
    }

    StringInputValidator.builder()
        .isOptional()
        .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getComments(), bindingResult);

    return bindingResult;
  }
}
