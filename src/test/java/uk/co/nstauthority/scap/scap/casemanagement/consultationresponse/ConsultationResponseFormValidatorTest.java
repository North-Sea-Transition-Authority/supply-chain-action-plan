package uk.co.nstauthority.scap.scap.casemanagement.consultationresponse;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ConsultationResponseFormValidatorTest {

  private ConsultationResponseForm form;

  private BindingResult bindingResult;

  @InjectMocks
  ConsultationResponseFormValidator validator;

  @Test
  void consultationResponseFormValidator_Supports() {
    assertTrue(validator.supports(ConsultationResponseForm.class));
    assertFalse(validator.supports(FurtherInfoRequestForm.class));
  }

  @Test
  void consultationResponseValidation_commentsNotPresent() {
    form = new ConsultationResponseForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("responseComments.inputValue").getDefaultMessage())
        .isEqualTo("Enter Comments");
  }

  @Test
  void consultationResponseValidation_isValid() {
    form = new ConsultationResponseForm();

    var input = form.getResponseComments();
    input.setInputValue("This is a Test String");
    form.setResponseComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }

  @Test
  void consultationResponseValidator_maxCharacter_invalid() {
    form = new ConsultationResponseForm();
    var input = form.getResponseComments();
    input.setInputValue("A".repeat(5000));
    form.setResponseComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);
    assertThat(extractedErrors).containsExactly(
        entry("responseComments.inputValue", Set.of("responseComments.maxCharExceeded")));
  }
}
