package uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse;

import java.util.ArrayList;
import java.util.List;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.file.FileUploadForm;

public class FurtherInfoResponseForm {

  private StringInput infoResponse = new StringInput("infoResponse", "Response comments");

  private List<FileUploadForm> infoResponseDocuments = new ArrayList<>();

  public StringInput getInfoResponse() {
    return infoResponse;
  }

  public void setInfoResponse(StringInput infoResponse) {
    this.infoResponse = infoResponse;
  }

  public List<FileUploadForm> getInfoResponseDocuments() {
    return infoResponseDocuments;
  }

  public void setInfoResponseDocuments(List<FileUploadForm> infoResponseDocuments) {
    this.infoResponseDocuments = infoResponseDocuments;
  }
}