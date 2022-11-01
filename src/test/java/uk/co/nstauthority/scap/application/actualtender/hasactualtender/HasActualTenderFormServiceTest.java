package uk.co.nstauthority.scap.application.actualtender.hasactualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;

@ExtendWith(MockitoExtension.class)
class HasActualTenderFormServiceTest {

  @Mock
  HasActualTenderFormValidator validator;

  @InjectMocks
  HasActualTenderFormService hasActualTenderFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new HasActualTenderForm();
    var inputBindingResult = new BeanPropertyBindingResult(form, "form");

    var outputBindingResult = hasActualTenderFormService.validate(form, inputBindingResult);

    assertThat(outputBindingResult).isEqualTo(inputBindingResult);
    verify(validator, times(1)).validate(form, inputBindingResult);
  }

  @Test
  void getForm_assertReturnsForm() {
    assertThat(hasActualTenderFormService.getForm()).isInstanceOf(HasActualTenderForm.class);
  }
}
