package uk.co.nstauthority.scap.permissionmanagement.teams;

import uk.co.fivium.formlibrary.input.DecimalInput;

public class NewTeamForm {

  private DecimalInput organisationGroupId;

  public DecimalInput getOrganisationGroupId() {
    return organisationGroupId;
  }

  public void setOrganisationGroupId(String organisationGroupId) {
    this.organisationGroupId.setInputValue(organisationGroupId);
  }

  public NewTeamForm() {
    this.organisationGroupId = new DecimalInput("organisationGroupId", "Organisation group");
  }
}
