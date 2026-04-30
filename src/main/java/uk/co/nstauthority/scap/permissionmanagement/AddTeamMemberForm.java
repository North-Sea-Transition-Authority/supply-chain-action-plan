package uk.co.nstauthority.scap.permissionmanagement;

import uk.co.fivium.formlibrary.input.StringInput;

public class AddTeamMemberForm {

  private StringInput email = new StringInput("email", "Email");

  public StringInput getEmail() {
    return email;
  }

  public void setEmail(StringInput email) {
    this.email = email;
  }
}
