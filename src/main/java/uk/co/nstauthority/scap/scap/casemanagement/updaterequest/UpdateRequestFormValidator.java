package uk.co.nstauthority.scap.scap.casemanagement.updaterequest;

import java.time.LocalDate;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.date.ThreeFieldDateInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class UpdateRequestFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return UpdateRequestForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (UpdateRequestForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(4000)
        .validate(form.getInfoRequest(), errors);

    ThreeFieldDateInputValidator.builder()
        .mustBeAfterDate(LocalDate.now())
        .validate(form.getDueDate(), errors);
  }
}
