package uk.co.nstauthority.scap.scap.casemanagement.consultationrequest;

import uk.co.fivium.formlibrary.input.StringInput;

public class ConsultationRequestForm {

  private StringInput requestComments = new StringInput("requestComments", "Request Comments");

  public StringInput getRequestComments() {
    return requestComments;
  }

  public void setRequestComments(StringInput requestComments) {
    this.requestComments = requestComments;
  }
}