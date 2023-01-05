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
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.EnergyPortalAccessService;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.IsMemberOfTeamOrRegulator;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForTeam;
import uk.co.nstauthority.scap.permissionmanagement.teams.RemoveMemberController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRemovalService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Controller
@IsMemberOfTeamOrRegulator
@RequestMapping("/permission-management/regulator/{teamId}/remove")
@PermissionsRequiredForTeam(permissions = RolePermission.GRANT_ROLES)
public class RegulatorRemoveMemberController extends RemoveMemberController {

  @Autowired
  public RegulatorRemoveMemberController(TeamService teamService,
                                         TeamMemberService teamMemberService,
                                         CustomerConfigurationProperties customerConfigurationProperties,
                                         TeamMemberViewService teamMemberViewService,
                                         TeamMemberRemovalService teamMemberRemovalService,
                                         UserDetailService userDetailService,
                                         EnergyPortalAccessService energyPortalAccessService) {
    super(teamService,
        teamMemberService,
        customerConfigurationProperties,
        teamMemberViewService,
        teamMemberRemovalService,
        userDetailService,
        energyPortalAccessService);
  }

  @Override
  @GetMapping("/{wuaId}")
  public ModelAndView renderRemoveMember(@PathVariable("teamId") TeamId teamId,
                                         @PathVariable("wuaId") WebUserAccountId wuaId) {
    return super.renderRemoveMember(teamId, wuaId)
        .addObject("backLinkUrl", ReverseRouter.route(on(RegulatorTeamManagementController.class)
            .renderMemberList(teamId)))
        .addObject("removeUrl", ReverseRouter.route(on(RegulatorRemoveMemberController.class)
            .removeMember(teamId, wuaId, null)));
  }

  @PostMapping("/{wuaId}")
  public ModelAndView removeMember(@PathVariable("teamId") TeamId teamId,
                                   @PathVariable("wuaId") WebUserAccountId wuaId,
                                   RedirectAttributes redirectAttributes) {
    return super.removeMember(teamId,
        wuaId,
        redirectAttributes,
        ReverseRouter.redirect(on(RegulatorTeamManagementController.class).renderMemberList(teamId)));
  }
}
