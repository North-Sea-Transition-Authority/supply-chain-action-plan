package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import com.google.common.annotations.VisibleForTesting;
import java.math.BigDecimal;
import java.time.Instant;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
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

  private Integer preferredBidderLocation;

  private Instant createdTimestamp;

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

  public Integer getPreferredBidderLocation() {
    return preferredBidderLocation;
  }

  public void setPreferredBidderLocation(Integer preferredBidderLocation) {
    this.preferredBidderLocation = preferredBidderLocation;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
