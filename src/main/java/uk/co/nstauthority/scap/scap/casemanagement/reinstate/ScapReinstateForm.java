package uk.co.nstauthority.scap.scap.casemanagement.reinstate;

import uk.co.fivium.formlibrary.input.StringInput;

public class ScapReinstateForm {

  private StringInput reinstateComments = new StringInput("reinstateComments", "reasons for reinstatement");

  public StringInput getReinstateComments() {
    return reinstateComments;
  }

  public void setReinstateComments(StringInput reinstateComments) {
    this.reinstateComments = reinstateComments;
  }
}