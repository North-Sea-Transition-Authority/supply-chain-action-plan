package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;

@ExtendWith(MockitoExtension.class)
class ActualTenderActivityFormServiceTest {

  @Mock
  ActualTenderActivityFormValidator actualTenderActivityFormValidator;

  @InjectMocks
  ActualTenderActivityFormService actualTenderActivityFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ActualTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = actualTenderActivityFormService.validate(form, bindingResult);

    verify(actualTenderActivityFormValidator).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
  }
}
