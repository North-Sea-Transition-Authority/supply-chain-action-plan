package uk.co.nstauthority.scap.permissionmanagement;

import java.util.Random;
import java.util.UUID;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class TeamTestUtil {

  private TeamTestUtil() {
    throw new IllegalUtilClassInstantiationException(this.getClass());
  }

  public static TeamBuilder Builder() {
    return new TeamBuilder();
  }

  public static class TeamBuilder {

    private UUID uuid = UUID.randomUUID();
    private TeamType teamType = TeamType.REGULATOR;

    private Integer orgGroupId = new Random().nextInt();

    private String teamName = "TestTeam";

    public TeamBuilder withId(UUID uuid) {
      this.uuid = uuid;
      return this;
    }

    public TeamBuilder withTeamType(TeamType teamType) {
      this.teamType = teamType;
      return this;
    }

    public TeamBuilder withTeamName(String teamName) {
      this.teamName = teamName;
      return this;
    }

    public Team build() {
      var team = new Team(uuid);
      team.setTeamType(teamType);
      team.setDisplayName(teamName);
      team.setEnergyPortalOrgGroupId(orgGroupId);
      if(teamType.equals(TeamType.REGULATOR)) {
        team.setEnergyPortalOrgGroupId(null);
      }
      return team;
    }
  }
}