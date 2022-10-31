package uk.co.nstauthority.scap.application.projectdetails;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
class ProjectDetailsFormService {

  private final ProjectDetailsFormValidator validator;

  @Autowired
  ProjectDetailsFormService(ProjectDetailsFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ProjectDetailsForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }
}
