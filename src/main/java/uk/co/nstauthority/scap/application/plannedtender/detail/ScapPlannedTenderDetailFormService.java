package uk.co.nstauthority.scap.application.plannedtender.detail;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
public class ScapPlannedTenderDetailFormService {

  private final ScapPlannedTenderDetailFormValidator validator;

  @Autowired
  public ScapPlannedTenderDetailFormService(ScapPlannedTenderDetailFormValidator validator) {
    this.validator = validator;
  }

  public BindingResult validate(BindingResult bindingResult, ScapPlannedTenderDetailForm form) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  public ScapPlannedTenderDetailForm getForm(ScapPlannedTenderDetail plannedTenderDetail) {
    var form = new ScapPlannedTenderDetailForm();
    form.setAwardRationale(plannedTenderDetail.getAwardRationale());
    form.setRemunerationModelName(plannedTenderDetail.getRemunerationModelName());
    form.setRemunerationModel(plannedTenderDetail.getRemunerationModel());
    form.setScopeDescription(plannedTenderDetail.getScopeDescription());
    form.setEstimatedValue(String.valueOf(plannedTenderDetail.getEstimatedValue()));
    return form;
  }
}
