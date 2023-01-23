package uk.co.nstauthority.scap.scap.casemanagement.furtherinfo;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class FurtherInfoRequestFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return FurtherInfoRequestForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (FurtherInfoRequestForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(4000)
        .validate(form.getInfoRequest(), errors);
  }
}
