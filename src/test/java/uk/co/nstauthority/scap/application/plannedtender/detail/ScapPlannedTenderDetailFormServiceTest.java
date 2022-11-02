package uk.co.nstauthority.scap.application.plannedtender.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
import java.time.Instant;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.application.RemunerationModel;

@ExtendWith(MockitoExtension.class)
class ScapPlannedTenderDetailFormServiceTest {

  @Mock
  ScapPlannedTenderDetailFormValidator validator;

  @InjectMocks
  ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ScapPlannedTenderDetailForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = scapPlannedTenderDetailFormService.validate(bindingResult, form);

    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    verify(validator, times(1)).validate(form, bindingResult);
  }

  @Test
  void getForm_assertReturnsFilledForm() {
    var plannedTenderDetail = new ScapPlannedTenderDetail(
        null,
        "some scope description",
        BigDecimal.valueOf(33.5),
        RemunerationModel.OTHER,
        "some remuneration model name",
        "some award rationale",
        Instant.now());

    var form = scapPlannedTenderDetailFormService.getForm(plannedTenderDetail);

    assertThat(form.getAwardRationale().getInputValue()).isEqualTo(plannedTenderDetail.getAwardRationale());
    assertThat(form.getScopeDescription().getInputValue()).isEqualTo(plannedTenderDetail.getScopeDescription());
    assertThat(form.getEstimatedValue().getAsBigDecimal()).contains(plannedTenderDetail.getEstimatedValue());
    assertThat(form.getRemunerationModel()).isEqualTo(plannedTenderDetail.getRemunerationModel());
    assertThat(form.getRemunerationModelName().getInputValue()).isEqualTo(plannedTenderDetail.getRemunerationModelName());
  }
}
