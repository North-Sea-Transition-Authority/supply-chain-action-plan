package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.IsMemberOfTeamOrRegulator;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Controller
@IsMemberOfTeamOrRegulator
@RequestMapping("/permission-management/regulator")
public class RegulatorTeamManagementController {

  private final TeamMemberViewService teamMemberViewService;
  private final RegulatorTeamService regulatorTeamService;
  private final UserDetailService userDetailService;
  private final CustomerConfigurationProperties customerConfigurationProperties;
  private final TeamMemberService teamMemberService;

  private final TeamService teamService;

  @Autowired
  RegulatorTeamManagementController(TeamMemberViewService teamMemberViewService,
                                    RegulatorTeamService regulatorTeamService,
                                    UserDetailService userDetailService,
                                    CustomerConfigurationProperties customerConfigurationProperties,
                                    TeamMemberService teamMemberService,
                                    TeamService teamService) {
    this.teamMemberViewService = teamMemberViewService;
    this.regulatorTeamService = regulatorTeamService;
    this.userDetailService = userDetailService;
    this.customerConfigurationProperties = customerConfigurationProperties;
    this.teamMemberService = teamMemberService;
    this.teamService = teamService;
  }

  @GetMapping
  public ModelAndView renderMemberListRedirect() {

    var team = regulatorTeamService.getTeam(userDetailService.getUserDetail());

    return ReverseRouter.redirect(on(RegulatorTeamManagementController.class)
        .renderMemberList(new TeamId(team.getUuid())));
  }

  @GetMapping("/{teamId}")
  @IsMemberOfTeamOrRegulator
  public ModelAndView renderMemberList(@PathVariable("teamId") TeamId teamId) {

    var team = teamService.getTeam(teamId);

    var user = userDetailService.getUserDetail();

    var modelAndView = new ModelAndView("scap/permissionmanagement/teamMembers")
        .addObject("pageTitle", "Manage %s".formatted(customerConfigurationProperties.mnemonic()))
        .addObject("teamName", customerConfigurationProperties.mnemonic())
        .addObject("teamRoles", RegulatorTeamRole.values())
        .addObject("teamMembers", teamMemberViewService.findTeamMemberViewsForTeam(team));

    if (regulatorTeamService.isAccessManager(teamId, userDetailService.getUserDetail())) {
      modelAndView
          .addObject("addTeamMemberUrl",
              ReverseRouter.route(on(RegulatorAddMemberController.class).renderAddTeamMember(teamId)))
          .addObject("canRemoveUsers", teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user,
              Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
          .addObject("canEditUsers", teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user,
              Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())));
    }

    return modelAndView;
  }
}
