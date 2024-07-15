package uk.co.nstauthority.scap.permissionmanagement;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.util.UUID;
import org.hibernate.annotations.UuidGenerator;
import org.hibernate.envers.Audited;

@Entity
@Table(name = "teams")
public class Team {

  @Id
  @UuidGenerator
  @Audited
  private UUID uuid;

  @Column(name = "type")
  @Enumerated(EnumType.STRING)
  private TeamType teamType;

  @Column(name = "display_name")
  @Audited
  private String displayName;

  @Column(name = "organisation_group_id")
  @Audited
  private Integer energyPortalOrgGroupId;

  public Team() {
  }

  public Team(UUID uuid) {
    this.uuid = uuid;
  }

  public UUID getUuid() {
    return uuid;
  }

  public TeamType getTeamType() {
    return teamType;
  }

  public void setTeamType(TeamType teamType) {
    this.teamType = teamType;
  }

  public String getDisplayName() {
    return displayName;
  }

  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  public Integer getEnergyPortalOrgGroupId() {
    return energyPortalOrgGroupId;
  }

  public void setEnergyPortalOrgGroupId(Integer groupId) {
    this.energyPortalOrgGroupId = groupId;
  }
}
