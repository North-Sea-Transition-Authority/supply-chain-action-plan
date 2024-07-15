package uk.co.nstauthority.scap.scap.actualtender.activity;

import com.google.common.annotations.VisibleForTesting;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import java.time.Instant;
import org.hibernate.annotations.CreationTimestamp;

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

  @CreationTimestamp
  private Instant createdTimestamp;

  private Boolean isBidParticipant;

  private Integer organisationUnitId;

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

  public void setId(Integer id) {
    this.id = id;
  }

  public ActualTenderActivity getActualTenderActivity() {
    return actualTenderActivity;
  }

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

  public Integer getOrganisationUnitId() {
    return organisationUnitId;
  }

  public void setOrganisationUnitId(Integer organisationUnitId) {
    this.organisationUnitId = organisationUnitId;
  }
}
