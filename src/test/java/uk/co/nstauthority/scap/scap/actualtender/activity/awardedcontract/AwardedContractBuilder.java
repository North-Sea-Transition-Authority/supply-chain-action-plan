package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

public class AwardedContractBuilder {

  private Integer id;
  private ActualTenderActivity actualTenderActivity;
  private InvitationToTenderParticipant preferredBidder;
  private BigDecimal awardValue = BigDecimal.valueOf(1.1);
  private String awardRationale = "Test award rationale";
  private Integer preferredBidderCountryId = 0;
  private Instant createdTimestamp = Instant.now();
  private LocalDate contractAwardDate = LocalDate.of(2000, 1, 1);
  private Integer paymentTerms = 30;
  private LocalDate forecastExecutionStartDate = LocalDate.of(2001, 1, 1);
  private LocalDate forecastExecutionEndDate = LocalDate.of(2002, 1, 1);

  public AwardedContractBuilder withId(Integer id) {
    this.id = id;
    return this;
  }

  public AwardedContractBuilder withActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    this.actualTenderActivity = actualTenderActivity;
    return this;
  }

  public AwardedContractBuilder withPreferredBidder(InvitationToTenderParticipant preferredBidder) {
    this.preferredBidder = preferredBidder;
    return this;
  }

  public AwardedContractBuilder withAwardValue(BigDecimal awardValue) {
    this.awardValue = awardValue;
    return this;
  }

  public AwardedContractBuilder withAwardRationale(String awardRationale) {
    this.awardRationale = awardRationale;
    return this;
  }

  public AwardedContractBuilder withPreferredBidderCountryId(Integer preferredBidderCountryId) {
    this.preferredBidderCountryId = preferredBidderCountryId;
    return this;
  }

  public AwardedContractBuilder withCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
    return this;
  }

  public AwardedContractBuilder withContractAwardDate(LocalDate contractAwardDate) {
    this.contractAwardDate = contractAwardDate;
    return this;
  }

  public AwardedContractBuilder withPaymentTerms(Integer paymentTerms) {
    this.paymentTerms = paymentTerms;
    return this;
  }

  public AwardedContractBuilder withForecastExecutionStartDate(LocalDate forecastExecutionStartDate) {
    this.forecastExecutionStartDate = forecastExecutionStartDate;
    return this;
  }

  public AwardedContractBuilder withForecastExecutionEndDate(LocalDate forecastExecutionEndDate) {
    this.forecastExecutionEndDate = forecastExecutionEndDate;
    return this;
  }

  public AwardedContract build() {
    var awardedContract = new AwardedContract(id);
    awardedContract.setActualTenderActivity(actualTenderActivity);
    awardedContract.setPreferredBidder(preferredBidder);
    awardedContract.setAwardValue(awardValue);
    awardedContract.setAwardRationale(awardRationale);
    awardedContract.setPreferredBidderCountryId(preferredBidderCountryId);
    awardedContract.setCreatedTimestamp(createdTimestamp);
    awardedContract.setContractAwardDate(contractAwardDate);
    awardedContract.setPaymentTerms(paymentTerms);
    awardedContract.setForecastExecutionStartDate(forecastExecutionStartDate);
    awardedContract.setForecastExecutionEndDate(forecastExecutionEndDate);
    return awardedContract;
  }

  public static AwardedContractBuilder newBuilder() {
    return new AwardedContractBuilder();
  }
}
