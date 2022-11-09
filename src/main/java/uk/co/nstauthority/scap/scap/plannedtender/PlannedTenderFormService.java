package uk.co.nstauthority.scap.scap.plannedtender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
class PlannedTenderFormService {

  private final PlannedTenderFormValidator validator;

  @Autowired
  PlannedTenderFormService(PlannedTenderFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(PlannedTenderForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  PlannedTenderForm getForm(PlannedTender plannedTender) {
    var form = new PlannedTenderForm();
    if (HasMorePlannedTenderActivities.NO.equals(plannedTender.getHasMorePlannedTenderActivities())) {
      form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    }
    return form;
  }
}
