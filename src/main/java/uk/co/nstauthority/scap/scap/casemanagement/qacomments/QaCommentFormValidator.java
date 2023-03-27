package uk.co.nstauthority.scap.scap.casemanagement.qacomments;

import static uk.co.nstauthority.scap.util.ValidationUtil.TEXT_AREA_STANDARD_LIMIT;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class QaCommentFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return QaCommentForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (QaCommentForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(TEXT_AREA_STANDARD_LIMIT)
        .isOptional()
        .validate(form.getQaComments(), errors);
  }
}
