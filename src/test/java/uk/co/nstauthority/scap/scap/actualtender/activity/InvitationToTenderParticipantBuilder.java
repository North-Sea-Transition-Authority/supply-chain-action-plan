package uk.co.nstauthority.scap.scap.actualtender.activity;

import java.time.Instant;

public class InvitationToTenderParticipantBuilder {

  private Instant createdTimestamp;
  private Integer id;
  private Boolean isBidParticipant;
  private String companyName;
  private ActualTenderActivity actualTenderActivity;
  private Integer organisationUnitId;

  public InvitationToTenderParticipantBuilder() {
    this.createdTimestamp = Instant.now();
    this.id = null;
    this.isBidParticipant = false;
    this.companyName = "test company name";
    this.actualTenderActivity = new ActualTenderActivity();
  }

  public InvitationToTenderParticipantBuilder withCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
    return this;
  }

  public InvitationToTenderParticipantBuilder withId(Integer id) {
    this.id = id;
    return this;
  }

  public InvitationToTenderParticipantBuilder withBidParticipant(Boolean bidParticipant) {
    isBidParticipant = bidParticipant;
    return this;
  }

  public InvitationToTenderParticipantBuilder withCompanyName(String companyName) {
    this.companyName = companyName;
    return this;
  }

  public InvitationToTenderParticipantBuilder withActualTenderActivity(
      ActualTenderActivity actualTenderActivity) {
    this.actualTenderActivity = actualTenderActivity;
    return this;
  }

  public InvitationToTenderParticipantBuilder withOrganisationUnitId(Integer organisationUnitId) {
    this.organisationUnitId = organisationUnitId;
    return this;
  }

  public InvitationToTenderParticipant build() {
    var invitationToTenderParticipant = new InvitationToTenderParticipant(id);
    invitationToTenderParticipant.setActualTenderActivity(actualTenderActivity);
    invitationToTenderParticipant.setCompanyName(companyName);
    invitationToTenderParticipant.setCreatedTimestamp(createdTimestamp);
    invitationToTenderParticipant.setBidParticipant(isBidParticipant);
    invitationToTenderParticipant.setOrganisationUnitId(organisationUnitId);
    return invitationToTenderParticipant;
  }

  public static InvitationToTenderParticipantBuilder newBuilder() {
    return new InvitationToTenderParticipantBuilder();
  }
}
