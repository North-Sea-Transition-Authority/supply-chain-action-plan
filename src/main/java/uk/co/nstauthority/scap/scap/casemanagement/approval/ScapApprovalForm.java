package uk.co.nstauthority.scap.scap.casemanagement.approval;

import uk.co.fivium.formlibrary.input.StringInput;

public class ScapApprovalForm {

  private StringInput approvalComments = new StringInput("approvalComments", "Approval comments");

  public StringInput getApprovalComments() {
    return approvalComments;
  }

  public void setApprovalComments(StringInput approvalComments) {
    this.approvalComments = approvalComments;
  }
}