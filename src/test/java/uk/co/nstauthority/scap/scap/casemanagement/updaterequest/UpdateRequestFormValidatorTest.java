package uk.co.nstauthority.scap.scap.casemanagement.updaterequest;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.Clock;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;

@ExtendWith(MockitoExtension.class)
class UpdateRequestFormValidatorTest {

  private UpdateRequestForm form;

  private BindingResult bindingResult;

  @InjectMocks
  UpdateRequestFormValidator validator;

  @Test
  void updateRequestFormValidator_Supports() {
    assertTrue(validator.supports(UpdateRequestForm.class));
    assertFalse(validator.supports(FurtherInfoRequestForm.class));
  }


  @Test
  void furtherInfoValidation_commentsNotPresent() {
    form = new UpdateRequestForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    var dateInput = form.getDueDate();
    dateInput.setDate(LocalDate.now().plusDays(5L));
    form.setDueDate(dateInput);

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("infoRequest.inputValue").getCode()).isEqualTo("infoRequest.required");
  }

  @Test
  void furtherInfoValidation_dateNotPresent() {
    form = new UpdateRequestForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    var input = form.getInfoRequest();
    input.setInputValue("This is a Test String");
    form.setInfoRequest(input);

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("dueDate.dayInput.inputValue").getCode()).isEqualTo("dueDate.dayInput.required");
  }

  @Test
  void furtherInfoValidation_dateInPast() {
    form = new UpdateRequestForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    var input = form.getInfoRequest();
    input.setInputValue("This is a Test String");
    form.setInfoRequest(input);

    var dateInput = form.getDueDate();
    dateInput.setDate(LocalDate.now().minusDays(5L));
    form.setDueDate(dateInput);

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("dueDate.dayInput.inputValue").getCode()).isEqualTo("dueDate.dayInput.minDateExclusiveNotMet");
  }

  @Test
  void furtherInfoValidation_isValid() {
    form = new UpdateRequestForm();

    var input = form.getInfoRequest();
    input.setInputValue("This is a Test String");
    form.setInfoRequest(input);

    var dateInput = form.getDueDate();
    dateInput.setDate(LocalDate.now().plusDays(5L));
    form.setDueDate(dateInput);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }
}
