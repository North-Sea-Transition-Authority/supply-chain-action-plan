package uk.co.nstauthority.scap.scap.casemanagement.withdraw;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse.FurtherInfoResponseForm;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse.FurtherInfoResponseFormValidator;

@ExtendWith(MockitoExtension.class)
class ScapWithrawalFormValidatorTest {

  private ScapWithdrawalForm form;

  private BindingResult bindingResult;

  @InjectMocks
  ScapWithdrawalFormValidator validator;

  @Test
  void scapWithdrawalFormValidator_Supports() {
    assertTrue(validator.supports(ScapWithdrawalForm.class));
    assertFalse(validator.supports(FurtherInfoRequestForm.class));
  }

  @Test
  void withdrawScapValidation_commentsNotPresent() {
    form = new ScapWithdrawalForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("withdrawComments.inputValue").getDefaultMessage())
        .isEqualTo("Enter Withdrawal comments");
  }

  @Test
  void withdrawScapValidation_isValid() {
    form = new ScapWithdrawalForm();

    var input = form.getWithdrawComments();
    input.setInputValue("This is a Test String");
    form.setWithdrawComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }

  @Test
  void withdrawScapValidator_maxCharacter_invalid() {
    form = new ScapWithdrawalForm();
    var input = form.getWithdrawComments();
    input.setInputValue("A".repeat(5000));
    form.setWithdrawComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("withdrawComments.inputValue").getDefaultMessage())
        .isEqualTo("Withdrawal comments must be 4000 characters or less");
  }
}
