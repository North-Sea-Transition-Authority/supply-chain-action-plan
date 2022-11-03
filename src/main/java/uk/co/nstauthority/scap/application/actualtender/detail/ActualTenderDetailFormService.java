package uk.co.nstauthority.scap.application.actualtender.detail;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
class ActualTenderDetailFormService {

  private final ActualTenderDetailFormValidator validator;

  @Autowired
  ActualTenderDetailFormService(ActualTenderDetailFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ActualTenderDetailForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }
}
