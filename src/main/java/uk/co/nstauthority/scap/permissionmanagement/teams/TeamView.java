package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamMemberController;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamMemberController;

public record TeamView(TeamId teamId, TeamType teamType, String DisplayName) {

  public static TeamView fromTeam(Team team) {
    var teamId = new TeamId(team.getUuid());
    return new TeamView(teamId, team.getTeamType(), team.getDisplayName());
  }

  public String manageUrl() {
    return switch (teamType) {
      case REGULATOR -> ReverseRouter.route(on(
          RegulatorTeamMemberController.class).renderMemberList(teamId));
      case INDUSTRY -> ReverseRouter.route(on(
          IndustryTeamMemberController.class).renderMemberList(teamId));
    };
  }
}
