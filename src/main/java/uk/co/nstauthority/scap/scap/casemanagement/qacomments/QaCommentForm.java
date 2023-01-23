package uk.co.nstauthority.scap.scap.casemanagement.qacomments;

import uk.co.fivium.formlibrary.input.StringInput;

public class QaCommentForm {

  private StringInput qaComments = new StringInput("qaComments", "QA Comments");

  public StringInput getQaComments() {
    return qaComments;
  }

  public void setQaComments(StringInput qaComments) {
    this.qaComments = qaComments;
  }
}