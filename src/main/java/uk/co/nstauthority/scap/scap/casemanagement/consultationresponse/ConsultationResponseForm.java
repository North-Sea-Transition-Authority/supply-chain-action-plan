package uk.co.nstauthority.scap.scap.casemanagement.consultationresponse;

import java.util.ArrayList;
import java.util.List;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.file.FileUploadForm;

public class ConsultationResponseForm {

  private StringInput responseComments = new StringInput("responseComments", "Comments");

  private List<FileUploadForm> supportingDocuments = new ArrayList<>();

  public StringInput getResponseComments() {
    return responseComments;
  }

  public void setResponseComments(StringInput responseComments) {
    this.responseComments = responseComments;
  }

  public List<FileUploadForm> getSupportingDocuments() {
    return supportingDocuments;
  }

  public void setSupportingDocuments(List<FileUploadForm> supportingDocuments) {
    this.supportingDocuments = supportingDocuments;
  }
}