package uk.co.nstauthority.scap.scap.casemanagement.reinstate;

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
        .mustHaveCharacterCountAtMost(4000)
        .validate(form.getReinstateComments(), errors);
  }
}
