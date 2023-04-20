package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER;

import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.IsMemberOfTeamOrRegulator;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;


@Controller
@IsMemberOfTeamOrRegulator
@RequestMapping("/permission-management/industry")
public class IndustryTeamMemberController {

  private final TeamMemberViewService teamMemberViewService;

  private final IndustryTeamService industryTeamService;

  private final TeamMemberService teamMemberService;

  private final TeamService teamService;
  private final UserDetailService userDetailService;

  @Autowired
  IndustryTeamMemberController(TeamMemberViewService teamMemberViewService,
                               IndustryTeamService industryTeamService,
                               TeamMemberService teamMemberService,
                               TeamService teamService,
                               UserDetailService userDetailService) {
    this.teamMemberViewService = teamMemberViewService;
    this.industryTeamService = industryTeamService;
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
  }

  @GetMapping("/{teamId}")
  public ModelAndView renderMemberList(@PathVariable("teamId") TeamId teamId) {

    var team = teamService.getTeam(teamId);
    var user = userDetailService.getUserDetail();
    var modelAndView = new ModelAndView("scap/permissionmanagement/teamMembers")
        .addObject("pageTitle", "Manage %s".formatted(team.getDisplayName()))
        .addObject("teamName", team.getDisplayName())
        .addObject("teamRoles", IndustryTeamRole.values())
        .addObject("teamMembers", teamMemberViewService.findTeamMemberViewsForTeam(team));

    if (industryTeamService.isAccessManager(teamId, user)) {
      modelAndView
          .addObject("addTeamMemberUrl",
              ReverseRouter.route(on(IndustryAddMemberController.class).renderAddTeamMember(teamId)))
          .addObject("canRemoveUsers", industryTeamService.isAccessManager(teamId, user))
          .addObject("canEditUsers", industryTeamService.isAccessManager(teamId, user));
    }
    if (isOrganisationAccessManager(user)) {
      modelAndView
          .addObject("removeTeamUrl",
              ReverseRouter.route(on(TeamManagementController.class).renderArchiveTeamConfirmation(teamId)));
    }
    return modelAndView;
  }

  private boolean isOrganisationAccessManager(ServiceUserDetail user) {
    var regulatorTeam = teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR)
        .stream()
        .findFirst();

    if (regulatorTeam.isPresent()) {
      var teamId = new TeamId(regulatorTeam.get().getUuid());
      return teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user, Set.of(ORGANISATION_ACCESS_MANAGER.name()));
    }
    return false;
  }
}
