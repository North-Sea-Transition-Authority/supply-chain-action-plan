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

  public PlannedTenderActivityForm getForm(PlannedTenderActivity plannedTenderDetail) {
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale(plannedTenderDetail.getAwardRationale());
    form.setRemunerationModelName(plannedTenderDetail.getRemunerationModelName());
    form.setRemunerationModel(plannedTenderDetail.getRemunerationModel());
    form.setScopeDescription(plannedTenderDetail.getScopeDescription());
    form.setEstimatedValue(String.valueOf(plannedTenderDetail.getEstimatedValue()));
    return form;
  }
}
