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
    var plannedTenderActivity = new PlannedTenderActivity(null, clock.instant());
    plannedTenderActivity.setScopeDescription("some scope description");
    plannedTenderActivity.setEstimatedValue(BigDecimal.valueOf(33.5));
    plannedTenderActivity.setRemunerationModel(RemunerationModel.OTHER);
    plannedTenderActivity.setRemunerationModelName("some remuneration model name");
    plannedTenderActivity.setAwardRationale("some award rationale");

    var form = plannedTenderActivityFormService.getForm(plannedTenderActivity);

    assertThat(form.getAwardRationale().getInputValue()).isEqualTo(plannedTenderActivity.getAwardRationale());
    assertThat(form.getScopeDescription().getInputValue()).isEqualTo(plannedTenderActivity.getScopeDescription());
    assertThat(form.getEstimatedValue().getAsBigDecimal()).contains(plannedTenderActivity.getEstimatedValue());
    assertThat(form.getRemunerationModel()).isEqualTo(plannedTenderActivity.getRemunerationModel());
    assertThat(form.getRemunerationModelName().getInputValue()).isEqualTo(plannedTenderActivity.getRemunerationModelName());
  }
}
