package uk.co.nstauthority.scap.scap.plannedtender.activity;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
public class PlannedTenderActivityFormService {

  private final PlannedTenderActivityFormValidator validator;

  @Autowired
  public PlannedTenderActivityFormService(PlannedTenderActivityFormValidator validator) {
    this.validator = validator;
  }

  public BindingResult validate(BindingResult bindingResult, PlannedTenderActivityForm form) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  public PlannedTenderActivityForm getForm(PlannedTenderActivity plannedTenderActivity) {
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale(plannedTenderActivity.getAwardRationale());
    form.setRemunerationModelName(plannedTenderActivity.getRemunerationModelName());
    form.setRemunerationModel(plannedTenderActivity.getRemunerationModel());
    form.setScopeDescription(plannedTenderActivity.getScopeDescription());
    form.setEstimatedValue(String.valueOf(plannedTenderActivity.getEstimatedValue()));
    form.setIndicativeActualTenderStartDate(plannedTenderActivity.getExpectedActualTenderStartDate());
    form.setIndicativeContractAwardDate(plannedTenderActivity.getExpectedContractAwardDate());
    return form;
  }
}
