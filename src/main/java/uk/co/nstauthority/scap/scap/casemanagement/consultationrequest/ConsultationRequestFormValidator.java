package uk.co.nstauthority.scap.scap.casemanagement.consultationrequest;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class ConsultationRequestFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return ConsultationRequestForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (ConsultationRequestForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(4000)
        .isOptional()
        .validate(form.getRequestComments(), errors);
  }
}
