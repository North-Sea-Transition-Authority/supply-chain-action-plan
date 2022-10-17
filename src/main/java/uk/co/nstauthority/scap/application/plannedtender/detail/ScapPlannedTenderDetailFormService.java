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
}
