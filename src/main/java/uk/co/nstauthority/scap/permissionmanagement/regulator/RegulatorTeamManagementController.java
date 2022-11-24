package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.IsMemberOfTeam;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;

@Controller
@RequestMapping("/permission-management/regulator")
public class RegulatorTeamManagementController {

  private final TeamMemberViewService teamMemberViewService;
  private final RegulatorTeamService regulatorTeamService;
  private final UserDetailService userDetailService;
  private final CustomerConfigurationProperties customerConfigurationProperties;
  private final TeamMemberService teamMemberService;

  @Autowired
  RegulatorTeamManagementController(TeamMemberViewService teamMemberViewService,
                                    RegulatorTeamService regulatorTeamService,
                                    UserDetailService userDetailService,
                                    CustomerConfigurationProperties customerConfigurationProperties,
                                    TeamMemberService teamMemberService) {
    this.teamMemberViewService = teamMemberViewService;
    this.regulatorTeamService = regulatorTeamService;
    this.userDetailService = userDetailService;
    this.customerConfigurationProperties = customerConfigurationProperties;
    this.teamMemberService = teamMemberService;
  }

  @GetMapping
  public ModelAndView renderMemberListRedirect() {

    var team = regulatorTeamService.getRegulatorTeamForUser(userDetailService.getUserDetail())
        .orElseThrow(() -> new ResponseStatusException(
            HttpStatus.FORBIDDEN,
            "User with ID %s is not a member of regulator team".formatted(userDetailService.getUserDetail())
        ));

    return ReverseRouter.redirect(on(RegulatorTeamManagementController.class)
        .renderMemberList(new TeamId(team.getUuid())));
  }

  @GetMapping("/{teamId}")
  @IsMemberOfTeam
  public ModelAndView renderMemberList(@PathVariable("teamId") TeamId teamId) {

    var team = getRegulatorTeam(teamId);

    var user = userDetailService.getUserDetail();

    var modelAndView = new ModelAndView("scap/permissionmanagement/regulator/regulatorTeamMembers")
        .addObject("pageTitle", "Manage %s".formatted(customerConfigurationProperties.mnemonic()))
        .addObject("teamName", customerConfigurationProperties.mnemonic())
        .addObject("teamRoles", RegulatorTeamRole.values())
        .addObject("teamMembers", teamMemberViewService.getTeamMemberViewsForTeam(team));

    if (regulatorTeamService.isAccessManager(teamId, userDetailService.getUserDetail())) {
      modelAndView
          //TODO: Add 'Add Team Member Functionality'
          //.addObject("addTeamMemberUrl",
          //    ReverseRouter.route(on(RegulatorAddMemberController.class).renderAddTeamMember(teamId)))
          .addObject("canRemoveUsers", teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user,
              Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())))
          .addObject("canEditUsers", teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId, user,
              Set.of(RegulatorTeamRole.ACCESS_MANAGER.name())));
    }

    return modelAndView;
  }

  private Team getRegulatorTeam(TeamId teamId) {
    return regulatorTeamService.getTeam(teamId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "No regulator team with ID %s found".formatted(teamId.uuid())
        ));
  }

}