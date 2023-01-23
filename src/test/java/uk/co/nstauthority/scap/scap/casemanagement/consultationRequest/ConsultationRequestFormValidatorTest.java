package uk.co.nstauthority.scap.scap.casemanagement.consultationRequest;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestFormValidator;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;

@ExtendWith(MockitoExtension.class)
class ConsultationRequestFormValidatorTest {

  private ConsultationRequestForm form;

  private BindingResult bindingResult;

  @InjectMocks
  ConsultationRequestFormValidator validator;

  @Test
  void consultationRequestFormValidator_Supports() {
    assertTrue(validator.supports(ConsultationRequestForm.class));
    assertFalse(validator.supports(FurtherInfoRequestForm.class));
  }

  @Test
  void consultationRequestFormValidator_commentsNotPresent() {
    form = new ConsultationRequestForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void consultationRequestFormValidator_isValid() {
    form = new ConsultationRequestForm();

    var input = form.getRequestComments();
    input.setInputValue("This is a Test String");
    form.setRequestComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }
}
