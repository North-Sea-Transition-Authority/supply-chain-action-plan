package uk.co.nstauthority.scap.scap.organisationgroup;

import uk.co.fivium.formlibrary.input.DecimalInput;

public class OrganisationGroupForm {

  private final DecimalInput organisationGroupId;

  public OrganisationGroupForm() {
    this.organisationGroupId = new DecimalInput("organisationGroupId", "Operator");
  }

  public DecimalInput getOrganisationGroupId() {
    return organisationGroupId;
  }

  public void setOrganisationGroupId(String organisationGroupId) {
    this.organisationGroupId.setInputValue(organisationGroupId);
  }
}
