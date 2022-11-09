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

  InvitationToTenderParticipant(ActualTenderActivity actualTenderActivity, Instant createdTimestamp) {
    this.actualTenderActivity = actualTenderActivity;
    this.createdTimestamp = createdTimestamp;
    this.isBidParticipant = false;
  }

  public Integer getId() {
    return id;
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

  public Boolean getBidParticipant() {
    return isBidParticipant;
  }

  public void setBidParticipant(Boolean bidParticipant) {
    isBidParticipant = bidParticipant;
  }
}
