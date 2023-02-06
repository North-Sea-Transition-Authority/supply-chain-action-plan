package uk.co.nstauthority.scap.scap.casemanagement.approval;

import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.enumutil.YesNo;

public class ScapApprovalForm {

  private StringInput approvalComments = new StringInput("approvalComments", "Approval comments");

  private YesNo projectClosedOut;

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
}