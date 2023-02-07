package uk.co.nstauthority.scap.scap.casemanagement.withdraw;

import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;

@Service
public class ScapWithdrawalFormValidator implements Validator {

  @Override
  public boolean supports(Class<?> clazz) {
    return ScapWithdrawalForm.class.equals(clazz);
  }

  @Override
  public void validate(Object target, Errors errors) {
    var form = (ScapWithdrawalForm) target;

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(4000)
        .validate(form.getWithdrawComments(), errors);
  }
}
