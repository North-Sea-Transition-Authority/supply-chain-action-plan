package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceFormServiceTest {

  @Mock
  ContractingPerformanceFormValidator validator;

  @InjectMocks
  ContractingPerformanceFormService contractingPerformanceFormService;

  @Test
  void validate_VerifyCallsValidator() {
    var form = new ContractingPerformanceForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var activity = new ActualTenderActivity(49);
    var argumentCaptor = ArgumentCaptor.forClass(ContractingPerformanceFormValidatorHint.class);

    var returnedBindingResult = contractingPerformanceFormService
        .validate(form, bindingResult, List.of(activity));

    verify(validator).validate(eq(form), eq(bindingResult), argumentCaptor.capture());

    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    assertThat(argumentCaptor.getValue().activityIds()).containsExactly(activity.getId());
  }

  @Test
  void getScopeTitlesMap() {
    var activity1 = new ActualTenderActivity(49);
    activity1.setScopeTitle("test scope title 1");
    var activity2 = new ActualTenderActivity(50);
    activity2.setScopeTitle("test scope title 2");

    var returnedScopeTitlesMap = contractingPerformanceFormService.getScopeTitlesMap(List.of(activity1, activity2));

    assertThat(returnedScopeTitlesMap).containsExactly(
        entry(String.valueOf(activity1.getId()), activity1.getScopeTitle()),
        entry(String.valueOf(activity2.getId()), activity2.getScopeTitle())
    );
  }

  @Test
  void getForm() {
    var actualTenderActivity = new ActualTenderActivity(62);
    var contractingPerformance = new ContractingPerformance(52);
    contractingPerformance.setActualTenderActivity(actualTenderActivity);
    contractingPerformance.setOutturnCost(BigDecimal.valueOf(6.53));
    contractingPerformance.setOutturnRationale("Some outturn");

    var form = contractingPerformanceFormService.getForm(contractingPerformance);

    assertThat(form).extracting(
        ContractingPerformanceForm::getActualTenderActivityId,
        form1 -> form1.getOutturnCost().getInputValue(),
        form1 -> form1.getOutturnRationale().getInputValue()
    ).containsExactly(
        actualTenderActivity.getId(),
        contractingPerformance.getOutturnCost().toPlainString(),
        contractingPerformance.getOutturnRationale()
    );
  }
}
