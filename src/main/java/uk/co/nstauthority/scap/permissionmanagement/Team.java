package uk.co.nstauthority.scap.permissionmanagement;

import java.util.UUID;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import org.hibernate.annotations.GenericGenerator;

@Entity
@Table(name = "teams")
public class Team {

  @Id
  @GeneratedValue(generator = "uuid")
  @GenericGenerator(name = "uuid", strategy = "uuid2")
  private UUID uuid;

  @Column(name = "type")
  @Enumerated(EnumType.STRING)
  private TeamType teamType;

  @Column(name = "display_name")
  private String displayName;

  @Column(name = "organisation_group_id")
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
