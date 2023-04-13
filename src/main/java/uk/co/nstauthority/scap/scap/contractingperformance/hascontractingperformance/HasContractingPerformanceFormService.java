package uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;

@Service
public class HasContractingPerformanceFormService {

  private final HasContractingPerformanceFormValidator validator;

  @Autowired
  HasContractingPerformanceFormService(HasContractingPerformanceFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(HasContractingPerformanceForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  HasContractingPerformanceForm getForm(ContractingPerformanceOverview contractingPerformanceOverview) {
    var form = new HasContractingPerformanceForm();
    if (Boolean.TRUE.equals(contractingPerformanceOverview.getHasContractingPerformance())) {
      form.setHasContractingPerformance(YesNo.YES);
    } else if (Boolean.FALSE.equals(contractingPerformanceOverview.getHasContractingPerformance())) {
      form.setHasContractingPerformance(YesNo.NO);
    }
    return form;
  }
}
