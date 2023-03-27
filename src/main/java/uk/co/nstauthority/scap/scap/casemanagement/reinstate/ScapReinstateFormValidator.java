package uk.co.nstauthority.scap.scap.casemanagement.reinstate;

import static uk.co.nstauthority.scap.util.ValidationUtil.TEXT_AREA_STANDARD_LIMIT;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class ScapReinstateFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return ScapReinstateForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (ScapReinstateForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getReinstateComments(), errors);
  }
}
