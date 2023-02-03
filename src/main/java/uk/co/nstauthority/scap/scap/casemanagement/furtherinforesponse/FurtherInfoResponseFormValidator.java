package uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class FurtherInfoResponseFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return FurtherInfoResponseForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (FurtherInfoResponseForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(4000)
        .validate(form.getInfoResponse(), errors);
  }
}
