package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;

@ExtendWith(MockitoExtension.class)
class HasMoreContractingPerformanceFormServiceTest {

  @Mock
  HasMoreContractingPerformanceFormValidator hasMoreContractingPerformanceFormValidator;

  @InjectMocks
  HasMoreContractingPerformanceFormService hasMoreContractingPerformanceFormService;

  @Test
  void validate_VerifyCallsValidator() {
    var form = new HasMoreContractingPerformanceForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = hasMoreContractingPerformanceFormService.validate(form, bindingResult);

    assertThat(returnedBindingResult).isEqualTo(bindingResult);

    verify(hasMoreContractingPerformanceFormValidator).validate(form, bindingResult);
  }

  @Test
  void getForm_NoMoreContractingPerformance_AssertFilled() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasMoreContractingPerformance(HasMoreContractingPerformance.NO);

    var form = hasMoreContractingPerformanceFormService.getForm(contractingPerformanceOverview);

    assertThat(form.getHasMoreContractingPerformance()).isEqualTo(HasMoreContractingPerformance.NO);
  }

  @Test
  void getForm_HasMoreContractingPerformance_AssertEmptyForm() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasMoreContractingPerformance(HasMoreContractingPerformance.YES_LATER);

    var form = hasMoreContractingPerformanceFormService.getForm(contractingPerformanceOverview);

    assertThat(form.getHasMoreContractingPerformance()).isNull();
  }

}
