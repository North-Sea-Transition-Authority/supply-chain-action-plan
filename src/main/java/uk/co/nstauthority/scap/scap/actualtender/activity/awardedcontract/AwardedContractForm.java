package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.time.LocalDate;
import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.IntegerInput;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;

public class AwardedContractForm {

  static final String CONTRACT_AWARD_DATE_FIELD = "contractAwardDate";
  static final String CONTRACT_START_DATE_FIELD = "contractStartDate";
  static final String CONTRACT_END_DATE_FIELD = "contractEndDate";

  private Integer preferredBidderId;
  private final DecimalInput awardValue;
  private final StringInput awardRationale;
  private Integer preferredBidderCountryId;
  private final ThreeFieldDateInput contractAwardDate;
  private PaymentTermsRadio paymentTermsRadio;
  private final IntegerInput otherPaymentTerm;
  private final ThreeFieldDateInput contractStartDate;
  private final ThreeFieldDateInput contractEndDate;

  public AwardedContractForm() {
    this.awardValue = new DecimalInput("awardValue", "Award value");
    this.awardRationale = new StringInput("awardRationale", "Award rationale");
    this.contractAwardDate = new ThreeFieldDateInput(CONTRACT_AWARD_DATE_FIELD, "Contract award date");
    this.otherPaymentTerm = new IntegerInput("otherPaymentTerm", "Days for payment terms");
    this.contractStartDate = new ThreeFieldDateInput(CONTRACT_START_DATE_FIELD, "Forecast execution start date");
    this.contractEndDate = new ThreeFieldDateInput(CONTRACT_END_DATE_FIELD, "Forecast execution completion date");
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

  public PaymentTermsRadio getPaymentTermsRadio() {
    return paymentTermsRadio;
  }

  public void setPaymentTermsRadio(PaymentTermsRadio paymentTermsRadio) {
    this.paymentTermsRadio = paymentTermsRadio;
  }

  public IntegerInput getOtherPaymentTerm() {
    return otherPaymentTerm;
  }

  public void setOtherPaymentTerm(String otherPaymentTerm) {
    this.otherPaymentTerm.setInputValue(otherPaymentTerm);
  }

  public ThreeFieldDateInput getContractStartDate() {
    return contractStartDate;
  }

  public void setContractStartDate(LocalDate contractStartDate) {
    this.contractStartDate.setDate(contractStartDate);
  }

  public ThreeFieldDateInput getContractEndDate() {
    return contractEndDate;
  }

  public void setContractEndDate(LocalDate contractEndDate) {
    this.contractEndDate.setDate(contractEndDate);
  }
}
