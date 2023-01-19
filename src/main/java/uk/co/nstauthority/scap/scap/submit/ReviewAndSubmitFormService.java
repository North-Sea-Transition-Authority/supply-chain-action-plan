package uk.co.nstauthority.scap.scap.submit;

import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
class ReviewAndSubmitFormService {

  private final ReviewAndSubmitFormValidator validator;

  ReviewAndSubmitFormService(ReviewAndSubmitFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ReviewAndSubmitForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  ReviewAndSubmitForm getForm(ScapDetail scapDetail) {
    var form = new ReviewAndSubmitForm();
    form.setApprovedByStakeholders(scapDetail.getApprovedByStakeholders());
    return form;
  }
}
