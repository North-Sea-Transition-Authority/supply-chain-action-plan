package uk.co.nstauthority.scap.scap.plannedtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.RemunerationModel;

@ExtendWith(MockitoExtension.class)
class PlannedTenderActivityFormServiceTest {

  @Mock
  PlannedTenderActivityFormValidator validator;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  PlannedTenderActivityFormService plannedTenderActivityFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new PlannedTenderActivityForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = plannedTenderActivityFormService.validate(bindingResult, form);

    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    verify(validator).validate(form, bindingResult);
  }

  @Test
  void getForm_assertReturnsFilledForm() {
    var plannedTenderDetail = new PlannedTenderActivity(
        null,
        "some scope description",
        BigDecimal.valueOf(33.5),
        RemunerationModel.OTHER,
        "some remuneration model name",
        "some award rationale",
        clock.instant());

    var form = plannedTenderActivityFormService.getForm(plannedTenderDetail);

    assertThat(form.getAwardRationale().getInputValue()).isEqualTo(plannedTenderDetail.getAwardRationale());
    assertThat(form.getScopeDescription().getInputValue()).isEqualTo(plannedTenderDetail.getScopeDescription());
    assertThat(form.getEstimatedValue().getAsBigDecimal()).contains(plannedTenderDetail.getEstimatedValue());
    assertThat(form.getRemunerationModel()).isEqualTo(plannedTenderDetail.getRemunerationModel());
    assertThat(form.getRemunerationModelName().getInputValue()).isEqualTo(plannedTenderDetail.getRemunerationModelName());
  }
}
