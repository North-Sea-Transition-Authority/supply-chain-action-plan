package uk.co.nstauthority.scap.application.actualtender.hasactualtender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
class HasActualTenderFormService {

  HasActualTenderFormValidator validator;

  @Autowired
  HasActualTenderFormService(HasActualTenderFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(HasActualTenderForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  HasActualTenderForm getForm() {
    // TODO: When adding data model, automatically fill out form using entity as parameter
    return new HasActualTenderForm();
  }
}
