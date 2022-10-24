package uk.co.nstauthority.scap.application.plannedtender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
class ScapPlannedTenderFormService {

  private final ScapPlannedTenderFormValidator validator;

  @Autowired
  ScapPlannedTenderFormService(ScapPlannedTenderFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ScapPlannedTenderForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  ScapPlannedTenderForm getForm(ScapPlannedTender scapPlannedTender) {
    var form = new ScapPlannedTenderForm();
    if (HasMorePlannedTenderActivities.NO.equals(scapPlannedTender.getHasMorePlannedTenderActivities())) {
      form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    }
    return form;
  }
}
