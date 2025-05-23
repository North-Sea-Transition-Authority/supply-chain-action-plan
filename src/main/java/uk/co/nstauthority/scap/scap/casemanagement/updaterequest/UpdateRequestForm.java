package uk.co.nstauthority.scap.scap.casemanagement.updaterequest;

import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;

public class UpdateRequestForm {

  private StringInput infoRequest = new StringInput("infoRequest", "application update required");

  private ThreeFieldDateInput dueDate =  new ThreeFieldDateInput(
      "dueDate",
      "update due by");

  public StringInput getInfoRequest() {
    return infoRequest;
  }

  public void setInfoRequest(StringInput infoRequest) {
    this.infoRequest = infoRequest;
  }

  public ThreeFieldDateInput getDueDate() {
    return dueDate;
  }

  public void setDueDate(ThreeFieldDateInput dueDate) {
    this.dueDate = dueDate;
  }
}