package uk.co.nstauthority.scap.permissionmanagement.teams;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import java.util.UUID;
import org.hibernate.annotations.UuidGenerator;
import org.hibernate.envers.Audited;
import org.hibernate.envers.RelationTargetAuditMode;
import uk.co.nstauthority.scap.permissionmanagement.Team;

@Entity
@Table(name = "team_member_roles")
public class TeamMemberRole {

  @Id
  @UuidGenerator
  @Audited
  private UUID uuid;

  @Audited(targetAuditMode = RelationTargetAuditMode.NOT_AUDITED)
  @JoinColumn(name = "team_id")
  @ManyToOne
  private Team team;

  @Audited
  private Long wuaId;

  @Audited
  private String role;

  protected TeamMemberRole() {
  }

  public TeamMemberRole(UUID uuid) {
    this.uuid = uuid;
  }

  public UUID getUuid() {
    return uuid;
  }

  public Team getTeam() {
    return team;
  }

  public void setTeam(Team team) {
    this.team = team;
  }

  public Long getWuaId() {
    return wuaId;
  }

  public void setWuaId(Long wuaId) {
    this.wuaId = wuaId;
  }

  public String getRole() {
    return role;
  }

  public void setRole(String role) {
    this.role = role;
  }
}
