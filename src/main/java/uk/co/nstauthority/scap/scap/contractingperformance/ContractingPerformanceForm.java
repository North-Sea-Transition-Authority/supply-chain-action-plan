package uk.co.nstauthority.scap.scap.contractingperformance;

import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.StringInput;

class ContractingPerformanceForm {

  private Integer actualTenderActivityId;
  private final DecimalInput outturnCost;
  private final StringInput outturnRationale;

  public ContractingPerformanceForm() {
    this.outturnCost = new DecimalInput("outturnCost", "the outturn cost");
    this.outturnRationale =
        new StringInput("outturnRationale", "why the outturn is greater than the award value");
  }

  public Integer getActualTenderActivityId() {
    return actualTenderActivityId;
  }

  public void setActualTenderActivityId(Integer actualTenderActivityId) {
    this.actualTenderActivityId = actualTenderActivityId;
  }

  public DecimalInput getOutturnCost() {
    return outturnCost;
  }

  public void setOutturnCost(String inputValue) {
    this.outturnCost.setInputValue(inputValue);
  }

  public StringInput getOutturnRationale() {
    return outturnRationale;
  }

  public void setOutturnRationale(String inputValue) {
    this.outturnRationale.setInputValue(inputValue);
  }
}
