package uk.co.nstauthority.scap.scap.casemanagement.approval;

import java.util.ArrayList;
import java.util.List;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.file.FileUploadForm;

public class ScapApprovalForm {

  private StringInput approvalComments = new StringInput("approvalComments", "no objection comments");

  private YesNo projectClosedOut;

  private List<FileUploadForm> approvalDocuments = new ArrayList<>();

  private StringInput decisionRationale = new StringInput("decisionRationale", "summary of decision rationale");

  private Boolean understandGuidance;

  public StringInput getApprovalComments() {
    return approvalComments;
  }

  public void setApprovalComments(StringInput approvalComments) {
    this.approvalComments = approvalComments;
  }

  public YesNo getProjectClosedOut() {
    return projectClosedOut;
  }

  public void setProjectClosedOut(YesNo projectClosedOut) {
    this.projectClosedOut = projectClosedOut;
  }

  public List<FileUploadForm> getApprovalDocuments() {
    return approvalDocuments;
  }

  public void setApprovalDocuments(List<FileUploadForm> approvalDocuments) {
    this.approvalDocuments = approvalDocuments;
  }

  public StringInput getDecisionRationale() {
    return decisionRationale;
  }

  public void setDecisionRationale(StringInput decisionRationale) {
    this.decisionRationale = decisionRationale;
  }

  public Boolean getUnderstandGuidance() {
    return understandGuidance;
  }

  public void setUnderstandGuidance(Boolean understandGuidance) {
    this.understandGuidance = understandGuidance;
  }
}