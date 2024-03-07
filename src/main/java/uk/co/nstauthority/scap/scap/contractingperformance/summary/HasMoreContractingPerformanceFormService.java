package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;

@Service
class HasMoreContractingPerformanceFormService {

  private final HasMoreContractingPerformanceFormValidator validator;

  @Autowired
  HasMoreContractingPerformanceFormService(HasMoreContractingPerformanceFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(HasMoreContractingPerformanceForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  HasMoreContractingPerformanceForm getForm(ContractingPerformanceOverview contractingPerformanceOverview) {
    var form = new HasMoreContractingPerformanceForm();
    if (HasMoreContractingPerformance.NO.equals(contractingPerformanceOverview.getHasMoreContractingPerformance())) {
      form.setHasMoreContractingPerformance(HasMoreContractingPerformance.NO);
    }
    return form;
  }
}
