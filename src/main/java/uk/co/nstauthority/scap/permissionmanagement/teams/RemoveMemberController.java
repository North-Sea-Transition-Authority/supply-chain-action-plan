package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamManagementController;
import uk.co.nstauthority.scap.util.DeletionSuccessBannerUtil;

public abstract class RemoveMemberController {

  private final TeamMemberService teamMemberService;
  private final CustomerConfigurationProperties customerConfigurationProperties;
  private final TeamMemberViewService teamMemberViewService;
  private final TeamMemberRemovalService teamMemberRemovalService;
  private final TeamService teamService;

  @Autowired
  protected RemoveMemberController(
      TeamService teamService,
      TeamMemberService teamMemberService,
      CustomerConfigurationProperties customerConfigurationProperties,
      TeamMemberViewService teamMemberViewService,
      TeamMemberRemovalService teamMemberRemovalService) {
    this.teamMemberService = teamMemberService;
    this.customerConfigurationProperties = customerConfigurationProperties;
    this.teamMemberViewService = teamMemberViewService;
    this.teamMemberRemovalService = teamMemberRemovalService;
    this.teamService = teamService;
  }

  public ModelAndView renderRemoveMember(@PathVariable("teamId") TeamId teamId,
                                         @PathVariable("wuaId") WebUserAccountId wuaId) {

    var team = teamService.getTeam(teamId);
    var teamMemberOptional = teamMemberService.findTeamMember(team, wuaId);

    if (teamMemberOptional.isEmpty()) {
      switch (team.getTeamType()) {
        case REGULATOR -> {
          return ReverseRouter.redirect(on(RegulatorTeamManagementController.class).renderMemberList(teamId));
        }
        case INDUSTRY -> {
          return ReverseRouter.redirect(on(IndustryTeamManagementController.class).renderMemberList(teamId));
        }
        default -> {
          return ReverseRouter.redirect(on(TeamManagementController.class).renderTeamList());
        }
      }

    }

    var teamMember = teamMemberOptional.get();
    var userView = teamMemberViewService.getTeamMemberViewOrThrow(teamMember);
    var teamName = customerConfigurationProperties.mnemonic();
    if (team.getTeamType().equals(TeamType.INDUSTRY)) {
      teamName = team.getDisplayName();
    }

    var canRemoveTeamMember = teamMemberRemovalService.canRemoveTeamMember(team, teamMember);

    return new ModelAndView("scap/permissionmanagement/removeTeamMember")
        .addObject("pageTitle",
            teamMemberRemovalService.getRemoveScreenPageTitle(teamName, userView, canRemoveTeamMember))
        .addObject("teamName", teamName)
        .addObject("teamMember", userView)
        .addObject("canRemoveTeamMember", canRemoveTeamMember);
  }

  public ModelAndView removeMember(@PathVariable("teamId") TeamId teamId,
                                   @PathVariable("wuaId") WebUserAccountId wuaId,
                                   RedirectAttributes redirectAttributes,
                                   ModelAndView successUrl) {

    var team = teamService.getTeam(teamId);
    var teamMember = teamMemberService.getTeamMember(team, wuaId);

    if (teamMemberRemovalService.canRemoveTeamMember(team, teamMember)) {
      teamMemberRemovalService.removeTeamMember(team, teamMember);
    } else {
      return renderRemoveMember(teamId, wuaId)
          .addObject("singleErrorMessage", TeamMemberRemovalService.LAST_ACCESS_MANAGER_ERROR_MESSAGE);
    }
    var userView = teamMemberViewService.getTeamMemberViewOrThrow(teamMember);

    DeletionSuccessBannerUtil.addRedirectionNotification(redirectAttributes,
        "%s has been removed from the team".formatted(userView.getDisplayName()));
    return successUrl;
  }
}
