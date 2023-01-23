package uk.co.nstauthority.scap.scap.casemanagement.furtherinfo;

import uk.co.fivium.formlibrary.input.StringInput;

public class FurtherInfoRequestForm {

  private StringInput infoRequest = new StringInput("infoRequest", "Further information required");

  public StringInput getInfoRequest() {
    return infoRequest;
  }

  public void setInfoRequest(StringInput infoRequest) {
    this.infoRequest = infoRequest;
  }
}