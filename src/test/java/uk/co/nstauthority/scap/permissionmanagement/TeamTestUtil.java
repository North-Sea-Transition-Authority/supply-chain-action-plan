package uk.co.nstauthority.scap.permissionmanagement;

import java.util.UUID;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class TeamTestUtil {

  public static TeamView createTeamView(Team team) {
    return new TeamView(new TeamId(team.getUuid()), team.getTeamType());
  }

  private TeamTestUtil() {
    throw new IllegalUtilClassInstantiationException(this.getClass());
  }

  public static TeamBuilder Builder() {
    return new TeamBuilder();
  }

  public static class TeamBuilder {

    private UUID uuid = UUID.randomUUID();
    private TeamType teamType = TeamType.REGULATOR;

    public TeamBuilder withId(UUID uuid) {
      this.uuid = uuid;
      return this;
    }

    public TeamBuilder withTeamType(TeamType teamType) {
      this.teamType = teamType;
      return this;
    }

    public Team build() {
      var team = new Team(uuid);
      team.setTeamType(teamType);
      return team;
    }
  }
}