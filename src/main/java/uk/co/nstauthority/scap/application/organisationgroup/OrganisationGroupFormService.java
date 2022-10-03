package uk.co.nstauthority.scap.application.organisationgroup;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.application.overview.ScapOverview;

@Service
public class OrganisationGroupFormService {

  private final OrganisationGroupFormValidator organisationGroupFormValidator;

  @Autowired
  public OrganisationGroupFormService(OrganisationGroupFormValidator organisationGroupFormValidator) {
    this.organisationGroupFormValidator = organisationGroupFormValidator;
  }

  public BindingResult validate(OrganisationGroupForm form, BindingResult bindingResult) {
    organisationGroupFormValidator.validate(form, bindingResult);
    return bindingResult;
  }

  public OrganisationGroupForm getForm(ScapOverview scap) {
    var form = new OrganisationGroupForm();
    form.setOrganisationGroupId(String.valueOf(scap.getOrganisationGroupId()));
    return form;
  }
}
