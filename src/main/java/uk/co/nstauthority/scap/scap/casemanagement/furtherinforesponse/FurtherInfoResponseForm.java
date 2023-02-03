package uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse;

import uk.co.fivium.formlibrary.input.StringInput;

public class FurtherInfoResponseForm {

  private StringInput infoResponse = new StringInput("infoResponse", "Response comments");

  public StringInput getInfoResponse() {
    return infoResponse;
  }

  public void setInfoResponse(StringInput infoResponse) {
    this.infoResponse = infoResponse;
  }
}