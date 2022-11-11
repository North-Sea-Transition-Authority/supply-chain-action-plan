package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.StringInput;

class AwardedContractForm {

  private Integer preferredBidderId;
  private final DecimalInput awardValue;
  private final StringInput awardRationale;
  private Integer preferredBidderLocation;

  public AwardedContractForm() {
    this.awardValue = new DecimalInput("awardValue", "Award value");
    this.awardRationale = new StringInput("awardRationale", "Award rationale");
  }

  public Integer getPreferredBidderId() {
    return preferredBidderId;
  }

  public void setPreferredBidderId(Integer preferredBidderId) {
    this.preferredBidderId = preferredBidderId;
  }

  public DecimalInput getAwardValue() {
    return awardValue;
  }

  public void setAwardValue(String awardValue) {
    this.awardValue.setInputValue(awardValue);
  }

  public StringInput getAwardRationale() {
    return awardRationale;
  }

  public void setAwardRationale(String awardRationale) {
    this.awardRationale.setInputValue(awardRationale);
  }

  public Integer getPreferredBidderLocation() {
    return preferredBidderLocation;
  }

  public void setPreferredBidderLocation(Integer preferredBidderLocation) {
    this.preferredBidderLocation = preferredBidderLocation;
  }
}
