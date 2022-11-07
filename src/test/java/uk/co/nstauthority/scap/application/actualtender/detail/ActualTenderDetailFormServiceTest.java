package uk.co.nstauthority.scap.application.actualtender.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;

@ExtendWith(MockitoExtension.class)
class ActualTenderDetailFormServiceTest {

  @Mock
  ActualTenderDetailFormValidator actualTenderDetailFormValidator;

  @InjectMocks
  ActualTenderDetailFormService actualTenderDetailFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ActualTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = actualTenderDetailFormService.validate(form, bindingResult);

    verify(actualTenderDetailFormValidator).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
  }
}
