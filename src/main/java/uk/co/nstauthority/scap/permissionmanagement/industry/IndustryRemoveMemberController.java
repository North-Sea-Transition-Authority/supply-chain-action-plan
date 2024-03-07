package uk.co.nstauthority.scap.permissionmanagement.industry;

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
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForTeam;
import uk.co.nstauthority.scap.endpointvalidation.annotations.IsMemberOfTeam;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.teams.RemoveMemberController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRemovalService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Controller
@IsMemberOfTeam(allowRegulatorAccess = true)
@RequestMapping("/permission-management/industry/{teamId}/remove/{wuaId}")
@HasAnyPermissionForTeam(allowRegulatorAccess = true, permissions = RolePermission.MANAGE_ORGANISATIONS)
public class IndustryRemoveMemberController extends RemoveMemberController {
  @Autowired
  public IndustryRemoveMemberController(TeamService teamService,
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
  @GetMapping
  public ModelAndView renderRemoveMember(@PathVariable("teamId") TeamId teamId,
                                         @PathVariable("wuaId") WebUserAccountId wuaId) {
    return super.renderRemoveMember(teamId, wuaId)
        .addObject("backLinkUrl", ReverseRouter.route(on(IndustryTeamMemberController.class)
            .renderMemberList(teamId)))
        .addObject("removeUrl", ReverseRouter.route(on(IndustryRemoveMemberController.class)
            .removeMember(teamId, wuaId, null)));
  }

  @PostMapping
  public ModelAndView removeMember(@PathVariable("teamId") TeamId teamId,
                                   @PathVariable("wuaId") WebUserAccountId wuaId,
                                   RedirectAttributes redirectAttributes) {
    return super.removeMember(teamId,
        wuaId,
        redirectAttributes,
        ReverseRouter.redirect(on(IndustryTeamMemberController.class).renderMemberList(teamId)));
  }
}
