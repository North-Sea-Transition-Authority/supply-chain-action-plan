package uk.co.nstauthority.scap.scap.casemanagement.furtherinfo;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;

@ExtendWith(MockitoExtension.class)
class FurtherInfoFormValidatorTest {

  private FurtherInfoRequestForm form;

  private BindingResult bindingResult;

  @InjectMocks
  FurtherInfoRequestFormValidator validator;

  @Test
  void furtherInfoValidation_commentsNotPresent() {
    form = new FurtherInfoRequestForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("infoRequest.inputValue").getCode()).isEqualTo("infoRequest.required");
  }

  @Test
  void furtherInfoValidation_isValid() {
    form = new FurtherInfoRequestForm();

    var input = form.getInfoRequest();
    input.setInputValue("This is a Test String");
    form.setInfoRequest(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }
}
