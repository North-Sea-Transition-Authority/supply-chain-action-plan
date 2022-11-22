package uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;

@ExtendWith(MockitoExtension.class)
class HasContractingPerformanceFormServiceTest {

  @Mock
  HasContractingPerformanceFormValidator hasContractingPerformanceFormValidator;

  @InjectMocks
  HasContractingPerformanceFormService hasContractingPerformanceFormService;

  @Test
  void validate_VerifyCallsValidator() {
    var form = new HasContractingPerformanceForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = hasContractingPerformanceFormService.validate(form, bindingResult);

    verify(hasContractingPerformanceFormValidator).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
  }

  @Test
  void getForm_Yes_AssertFills() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(true);

    var form = hasContractingPerformanceFormService.getForm(contractingPerformanceOverview);

    assertThat(form.getHasContractingPerformance()).isEqualTo(YesNo.YES);
  }

  @Test
  void getForm_No_AssertFills() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(false);

    var form = hasContractingPerformanceFormService.getForm(contractingPerformanceOverview);

    assertThat(form.getHasContractingPerformance()).isEqualTo(YesNo.NO);
  }

  @Test
  void getForm_NullHasContractingPerformance_AssertNull() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(null);

    var form = hasContractingPerformanceFormService.getForm(contractingPerformanceOverview);

    assertThat(form.getHasContractingPerformance()).isNull();
  }
}
