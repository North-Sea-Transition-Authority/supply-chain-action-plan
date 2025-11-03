package uk.co.nstauthority.scap.permissionmanagement;


import java.util.UUID;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRole;

public class TeamMemberRoleTestUtil {

  public static Builder newBuilder() {
    return new Builder();
  }

  public static class Builder {

    private UUID id = UUID.randomUUID();
    private Team team = TeamTestUtil.Builder().build();
    private String role = "TEAM_MANAGER";
    private Long wuaId = 1L;

    public Builder withId(UUID id) {
      this.id = id;
      return this;
    }

    public Builder withTeam(Team team) {
      this.team = team;
      return this;
    }

    public Builder withRole(String role) {
      this.role = role;
      return this;
    }

    public Builder withWuaId(Long wuaId) {
      this.wuaId = wuaId;
      return this;
    }

    public TeamMemberRole build() {
      var teamRole = new TeamMemberRole(id);
      teamRole.setTeam(team);
      teamRole.setRole(role);
      teamRole.setWuaId(wuaId);

      return teamRole;
    }

    private Builder() {
    }

  }

}