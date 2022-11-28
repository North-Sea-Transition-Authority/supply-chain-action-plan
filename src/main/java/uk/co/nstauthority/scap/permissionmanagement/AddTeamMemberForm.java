package uk.co.nstauthority.scap.permissionmanagement;

import uk.co.fivium.formlibrary.input.StringInput;

public class AddTeamMemberForm {

  private StringInput username = new StringInput("username", "Username");

  public StringInput getUsername() {
    return username;
  }

  public void setUsername(StringInput username) {
    this.username = username;
  }
}