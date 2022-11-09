package uk.co.nstauthority.scap.scap.actualtender.activity;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
class ActualTenderActivityFormService {

  private final ActualTenderActivityFormValidator validator;

  @Autowired
  ActualTenderActivityFormService(ActualTenderActivityFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ActualTenderActivityForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }
}
