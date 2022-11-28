package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.util.DeletionSuccessBannerUtil;

@Controller
@RequestMapping("/permission-management/regulator/{teamId}/remove")
@RegulatorRolesAllowed(roles = {RegulatorTeamRole.ACCESS_MANAGER})
public class RegulatorRemoveMemberController extends AbstractRegulatorPermissionManagement {

  private final TeamMemberService teamMemberService;
  private final CustomerConfigurationProperties customerConfigurationProperties;
  private final TeamMemberViewService teamMemberViewService;
  private final RegulatorTeamMemberRemovalService regulatorTeamMemberRemovalService;

  @Autowired
  public RegulatorRemoveMemberController(
      RegulatorTeamService regulatorTeamService,
      TeamMemberService teamMemberService,
      CustomerConfigurationProperties customerConfigurationProperties,
      TeamMemberViewService teamMemberViewService,
      RegulatorTeamMemberRemovalService regulatorTeamMemberRemovalService) {
    super(regulatorTeamService);
    this.teamMemberService = teamMemberService;
    this.customerConfigurationProperties = customerConfigurationProperties;
    this.teamMemberViewService = teamMemberViewService;
    this.regulatorTeamMemberRemovalService = regulatorTeamMemberRemovalService;
  }

  @GetMapping("/{wuaId}")
  public ModelAndView renderRemoveMember(@PathVariable("teamId") TeamId teamId,
                                         @PathVariable("wuaId") WebUserAccountId wuaId) {

    var team = getRegulatorTeam(teamId);

    var teamMemberOptional = teamMemberService.getTeamMember(team, wuaId);

    if (teamMemberOptional.isEmpty()) {
      return ReverseRouter.redirect(on(
          RegulatorTeamManagementController.class).renderMemberList(teamId));
    }

    var teamMember = teamMemberOptional.get();

    var userView = teamMemberViewService.getTeamMemberViewOrThrow(teamMember);

    var teamName = customerConfigurationProperties.mnemonic();
    var canRemoveTeamMember = regulatorTeamMemberRemovalService.canRemoveTeamMember(team, teamMember);

    return new ModelAndView("scap/permissionmanagement/regulator/regulatorRemoveTeamMember")
        .addObject("pageTitle",
            regulatorTeamMemberRemovalService.getRemoveScreenPageTitle(teamName, userView, canRemoveTeamMember))
        .addObject("teamName", teamName)
        .addObject("teamMember", userView)
        .addObject("backLinkUrl", ReverseRouter.route(on(RegulatorTeamManagementController.class)
            .renderMemberList(teamId)))
        .addObject("removeUrl", ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .removeMember(teamId, wuaId, null)))
        .addObject("canRemoveTeamMember", canRemoveTeamMember);
  }

  @PostMapping("/{wuaId}")
  public ModelAndView removeMember(@PathVariable("teamId") TeamId teamId,
                                   @PathVariable("wuaId") WebUserAccountId wuaId,
                                   RedirectAttributes redirectAttributes) {

    var team = getRegulatorTeam(teamId);

    var teamMember = teamMemberService.getTeamMemberOrThrow(team, wuaId);

    if (regulatorTeamMemberRemovalService.canRemoveTeamMember(team, teamMember)) {
      regulatorTeamMemberRemovalService.removeTeamMember(team, teamMember);
    } else {
      return renderRemoveMember(teamId, wuaId)
          .addObject("singleErrorMessage", RegulatorTeamMemberRemovalService.LAST_ACCESS_MANAGER_ERROR_MESSAGE);
    }

    var userView = teamMemberViewService.getTeamMemberViewOrThrow(teamMember);

    DeletionSuccessBannerUtil.addRedirectionNotification(redirectAttributes,
        "%s has been removed from the team".formatted(userView.getDisplayName()));

    return ReverseRouter.redirect(on(RegulatorTeamManagementController.class).renderMemberList(teamId));

  }
}
