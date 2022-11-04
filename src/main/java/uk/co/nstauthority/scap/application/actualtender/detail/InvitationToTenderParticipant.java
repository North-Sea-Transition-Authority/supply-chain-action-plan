package uk.co.nstauthority.scap.application.actualtender.detail;

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
class InvitationToTenderParticipant {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @ManyToOne
  @JoinColumn(name = "actual_tender_activity_id")
  private ActualTenderDetail actualTenderDetail;

  private String companyName;

  private Instant createdTimestamp;

  public InvitationToTenderParticipant() {
  }

  InvitationToTenderParticipant(ActualTenderDetail actualTenderDetail, Instant createdTimestamp) {
    this.actualTenderDetail = actualTenderDetail;
    this.createdTimestamp = createdTimestamp;
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
}
