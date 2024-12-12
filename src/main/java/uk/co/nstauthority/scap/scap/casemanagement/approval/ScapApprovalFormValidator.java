package uk.co.nstauthority.scap.scap.casemanagement.approval;

import static uk.co.nstauthority.scap.util.ValidationUtil.TEXT_AREA_STANDARD_LIMIT;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class ScapApprovalFormValidator implements Validator {

  private static final String UNDERSTAND_GUIDANCE_FIELD_NAME = "understandGuidance";
  private static final String UNDERSTAND_GUIDANCE_ERROR_CODE = "%s.notFound".formatted(UNDERSTAND_GUIDANCE_FIELD_NAME);
  private static final String UNDERSTAND_GUIDANCE_ERROR_MESSAGE = "You must understand what no objection means.";

  @Override
  public boolean supports(Class<?> clazz) {
    return ScapApprovalForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (ScapApprovalForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getApprovalComments(), errors);

    ValidationUtils.rejectIfEmpty(
        errors,
        "projectClosedOut",
        "projectClosedOut.required",
        "You must declare if the project has been completed."
    );

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(300)
        .validate(form.getDecisionRationale(), errors);

    if (!Boolean.TRUE.equals(form.getUnderstandGuidance())) {
      errors.rejectValue(
          UNDERSTAND_GUIDANCE_FIELD_NAME,
          UNDERSTAND_GUIDANCE_ERROR_CODE,
          UNDERSTAND_GUIDANCE_ERROR_MESSAGE
      );
    }
  }
}
