package uk.co.nstauthority.scap.scap.actualtender.summary;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@Service
class ActualTenderSummaryFormService {

  private final ActualTenderSummaryFormValidator validator;

  @Autowired
  ActualTenderSummaryFormService(ActualTenderSummaryFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ActualTenderSummaryForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  ActualTenderSummaryForm getForm(ActualTender actualTender) {
    var form = new ActualTenderSummaryForm();
    if (HasMoreActualTenderActivities.NO.equals(actualTender.getHasMoreActualTenders())) {
      form.setHasMoreActualTenderActivities(actualTender.getHasMoreActualTenders());
    }
    return form;
  }

}
