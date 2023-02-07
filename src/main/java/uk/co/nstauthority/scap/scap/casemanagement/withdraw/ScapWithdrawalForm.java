package uk.co.nstauthority.scap.scap.casemanagement.withdraw;

import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.nstauthority.scap.enumutil.YesNo;

public class ScapWithdrawalForm {

  private StringInput withdrawComments = new StringInput("withdrawComments", "Withdrawal comments");

  public StringInput getWithdrawComments() {
    return withdrawComments;
  }

  public void setWithdrawComments(StringInput withdrawComments) {
    this.withdrawComments = withdrawComments;
  }
}