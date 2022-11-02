package uk.co.nstauthority.scap.application.actualtender.hasactualtender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.application.actualtender.ActualTender;
import uk.co.nstauthority.scap.enumutil.YesNo;

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

  HasActualTenderForm getForm(ActualTender actualTender) {
    var form = new HasActualTenderForm();
    if (Boolean.TRUE.equals(actualTender.getHasActualTenders())) {
      form.setHasActualTender(YesNo.YES);
    } else if (Boolean.FALSE.equals(actualTender.getHasActualTenders())) {
      form.setHasActualTender(YesNo.NO);
    }
    return form;
  }
}
