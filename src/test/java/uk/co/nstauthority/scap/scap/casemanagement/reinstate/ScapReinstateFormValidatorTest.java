package uk.co.nstauthority.scap.scap.casemanagement.reinstate;

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

@ExtendWith(MockitoExtension.class)
class ScapReinstateFormValidatorTest {

  private ScapReinstateForm form;

  private BindingResult bindingResult;

  @InjectMocks
  ScapReinstateFormValidator validator;

  @Test
  void scapWithdrawalFormValidator_Supports() {
    assertTrue(validator.supports(ScapReinstateForm.class));
    assertFalse(validator.supports(FurtherInfoRequestForm.class));
  }

  @Test
  void reinstateScapValidation_commentsNotPresent() {
    form = new ScapReinstateForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("reinstateComments.inputValue").getDefaultMessage())
        .isEqualTo("Enter reasons for reinstatement");
  }

  @Test
  void reinstateScapValidation_isValid() {
    form = new ScapReinstateForm();

    var input = form.getReinstateComments();
    input.setInputValue("This is a Test String");
    form.setReinstateComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }

  @Test
  void reinstateScapValidator_maxCharacter_invalid() {
    form = new ScapReinstateForm();
    var input = form.getReinstateComments();
    input.setInputValue("A".repeat(5000));
    form.setReinstateComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("reinstateComments.inputValue").getDefaultMessage())
        .isEqualTo("Reasons for reinstatement must be 4000 characters or less");
  }
}
