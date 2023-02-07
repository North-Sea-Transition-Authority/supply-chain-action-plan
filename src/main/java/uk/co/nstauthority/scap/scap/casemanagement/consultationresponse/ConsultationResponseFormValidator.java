package uk.co.nstauthority.scap.scap.casemanagement.consultationresponse;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class ConsultationResponseFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return ConsultationResponseForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (ConsultationResponseForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(4000)
        .validate(form.getResponseComments(), errors);
  }
}
