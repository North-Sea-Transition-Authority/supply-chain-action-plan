package uk.co.nstauthority.scap.scap.actualtender.activity;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "invitation_to_tender_participants")
public class InvitationToTenderParticipant {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "actual_tender_activity_id")
  private ActualTenderActivity actualTenderActivity;

  private String companyName;

  private Instant createdTimestamp;

  private Boolean isBidParticipant;

  public InvitationToTenderParticipant() {
  }

  @VisibleForTesting
  public InvitationToTenderParticipant(Integer id) {
    this.id = id;
  }

  public InvitationToTenderParticipant(ActualTenderActivity actualTenderActivity, Instant createdTimestamp,
                                       String companyName) {
    this.actualTenderActivity = actualTenderActivity;
    this.createdTimestamp = createdTimestamp;
    this.isBidParticipant = false;
    this.companyName = companyName;
  }

  public Integer getId() {
    return id;
  }

  public ActualTenderActivity getActualTenderActivity() {
    return actualTenderActivity;
  }

  @VisibleForTesting
  public void setActualTenderActivity(
      ActualTenderActivity actualTenderActivity) {
    this.actualTenderActivity = actualTenderActivity;
  }

  public String getCompanyName() {
    return companyName;
  }

  public void setCompanyName(String companyName) {
    this.companyName = companyName;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }

  @VisibleForTesting
  void setCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
  }

  public Boolean getBidParticipant() {
    return isBidParticipant;
  }

  public void setBidParticipant(Boolean bidParticipant) {
    isBidParticipant = bidParticipant;
  }
}
