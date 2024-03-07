package uk.co.nstauthority.scap.permissionmanagement;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamView;

public class TeamMemberTestUtil {

  private TeamMemberTestUtil() {
    throw new IllegalUtilClassInstantiationException(this.getClass());
  }

  public static TeamMemberBuilder Builder() {
    return new TeamMemberBuilder();
  }

  public static class TeamMemberBuilder {

    private WebUserAccountId webUserAccountId = new WebUserAccountId(123L);
    private Set<TeamRole> teamRoles = new HashSet<>();
    private TeamId teamId = new TeamId(UUID.randomUUID());
    private TeamType teamType = TeamType.REGULATOR;

    private String teamName = "Test Team";

    public TeamMemberBuilder withWebUserAccountId(long webUserAccountId) {
      this.webUserAccountId = new WebUserAccountId(webUserAccountId);
      return this;
    }

    public TeamMemberBuilder withTeamId(TeamId teamId) {
      this.teamId = teamId;
      return this;
    }

    public TeamMemberBuilder withTeamType(TeamType teamType) {
      this.teamType = teamType;
      return this;
    }

    public TeamMemberBuilder withRole(TeamRole teamRole) {
      teamRoles.add(teamRole);
      return this;
    }

    public TeamMemberBuilder withRoles(Set<TeamRole> teamRoles) {
      this.teamRoles = teamRoles;
      return this;
    }

    public TeamMember build() {
      if (teamRoles.isEmpty()) {
        teamRoles.add(RegulatorTeamRole.ACCESS_MANAGER);
      }
      return new TeamMember(webUserAccountId, new TeamView(teamId, teamType, teamName), teamRoles);
    }

  }
}
