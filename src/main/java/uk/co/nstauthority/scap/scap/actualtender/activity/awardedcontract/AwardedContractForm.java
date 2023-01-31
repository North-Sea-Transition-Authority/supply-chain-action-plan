package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.time.LocalDate;
import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;

class AwardedContractForm {

  static final String CONTRACT_AWARD_DATE_FIELD = "contractAwardDate";

  private Integer preferredBidderId;
  private final DecimalInput awardValue;
  private final StringInput awardRationale;
  private Integer preferredBidderCountryId;
  private final ThreeFieldDateInput contractAwardDate;

  public AwardedContractForm() {
    this.awardValue = new DecimalInput("awardValue", "Award value");
    this.awardRationale = new StringInput("awardRationale", "Award rationale");
    this.contractAwardDate = new ThreeFieldDateInput(CONTRACT_AWARD_DATE_FIELD, "Contract award date");
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

  public Integer getPreferredBidderCountryId() {
    return preferredBidderCountryId;
  }

  public void setPreferredBidderCountryId(Integer preferredBidderCountryId) {
    this.preferredBidderCountryId = preferredBidderCountryId;
  }

  public ThreeFieldDateInput getContractAwardDate() {
    return contractAwardDate;
  }

  public void setContractAwardDate(LocalDate localDate) {
    this.contractAwardDate.setDate(localDate);
  }
}
