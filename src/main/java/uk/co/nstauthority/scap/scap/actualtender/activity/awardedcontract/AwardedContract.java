package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import com.google.common.annotations.VisibleForTesting;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import org.hibernate.annotations.CreationTimestamp;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@Entity
@Table(name = "awarded_contracts")
public class AwardedContract {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "actual_tender_activity_id")
  private ActualTenderActivity actualTenderActivity;

  @OneToOne(cascade = CascadeType.REMOVE)
  @JoinColumn(name = "preferred_bidder_ittp_id")
  private InvitationToTenderParticipant preferredBidder;

  private BigDecimal awardValue;

  private String awardRationale;

  private Integer preferredBidderCountryId;

  @CreationTimestamp
  private Instant createdTimestamp;

  private LocalDate contractAwardDate;

  private Integer paymentTerms;

  private LocalDate forecastExecutionStartDate;

  private LocalDate forecastExecutionEndDate;

  public AwardedContract() {
  }

  @VisibleForTesting
  AwardedContract(Integer id) {
    this.id = id;
  }

  public AwardedContract(ActualTenderActivity actualTenderActivity, Instant createdTimestamp) {
    this.actualTenderActivity = actualTenderActivity;
    this.createdTimestamp = createdTimestamp;
  }

  public Integer getId() {
    return id;
  }

  public void setId(Integer id) {
    this.id = id;
  }

  public ActualTenderActivity getActualTenderActivity() {
    return actualTenderActivity;
  }

  public InvitationToTenderParticipant getPreferredBidder() {
    return preferredBidder;
  }

  public void setPreferredBidder(
      InvitationToTenderParticipant preferredBidder) {
    this.preferredBidder = preferredBidder;
  }

  public BigDecimal getAwardValue() {
    return awardValue;
  }

  public void setAwardValue(BigDecimal awardValue) {
    this.awardValue = awardValue;
  }

  public String getAwardRationale() {
    return awardRationale;
  }

  public void setAwardRationale(String awardRationale) {
    this.awardRationale = awardRationale;
  }

  public Integer getPreferredBidderCountryId() {
    return preferredBidderCountryId;
  }

  public void setPreferredBidderCountryId(Integer preferredBidderCountryId) {
    this.preferredBidderCountryId = preferredBidderCountryId;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  public LocalDate getContractAwardDate() {
    return contractAwardDate;
  }

  public void setContractAwardDate(LocalDate contractAwardDate) {
    this.contractAwardDate = contractAwardDate;
  }

  public Integer getPaymentTerms() {
    return paymentTerms;
  }

  public void setPaymentTerms(Integer paymentTerms) {
    this.paymentTerms = paymentTerms;
  }

  public LocalDate getForecastExecutionStartDate() {
    return forecastExecutionStartDate;
  }

  public void setForecastExecutionStartDate(LocalDate forecastExecutionStartDate) {
    this.forecastExecutionStartDate = forecastExecutionStartDate;
  }

  public LocalDate getForecastExecutionEndDate() {
    return forecastExecutionEndDate;
  }

  public void setForecastExecutionEndDate(LocalDate forecastExecutionEndDate) {
    this.forecastExecutionEndDate = forecastExecutionEndDate;
  }

  public void setActualTenderActivity(
      ActualTenderActivity actualTenderActivity) {
    this.actualTenderActivity = actualTenderActivity;
  }

  public void setCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
  }
}
