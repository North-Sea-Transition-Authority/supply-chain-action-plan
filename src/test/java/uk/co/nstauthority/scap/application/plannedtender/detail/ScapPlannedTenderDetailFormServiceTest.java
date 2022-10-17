package uk.co.nstauthority.scap.application.plannedtender.detail;

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
public class ScapPlannedTenderDetailFormServiceTest {

  @Mock
  ScapPlannedTenderDetailFormValidator validator;

  @InjectMocks
  ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService;

  @Test
  public void validate_verifyCallsValidator() {
    var form = new ScapPlannedTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = scapPlannedTenderDetailFormService.validate(bindingResult, form);

    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    verify(validator, times(1)).validate(form, bindingResult);
  }
}
