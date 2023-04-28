package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.IsMemberOfTeam;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberView;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamRoleUtil;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForTeam;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Controller
@IsMemberOfTeam
@RequestMapping("/permission-management/regulator/{teamId}/edit")
@PermissionsRequiredForTeam(permissions = RolePermission.GRANT_ROLES)
public class RegulatorEditMemberController {
  private final TeamMemberService teamMemberService;
  private final TeamMemberViewService teamMemberViewService;
  private final ControllerHelperService controllerHelperService;
  private final RegulatorTeamMemberEditRolesValidator regulatorTeamMemberEditRolesValidator;
  private final TeamMemberRoleService teamMemberRoleService;

  private final TeamService teamService;

  @Autowired
  RegulatorEditMemberController(
      TeamMemberService teamMemberService,
      TeamMemberViewService teamMemberViewService,
      ControllerHelperService controllerHelperService,
      RegulatorTeamMemberEditRolesValidator regulatorTeamMemberEditRolesValidator,
      TeamMemberRoleService teamMemberRoleService,
      TeamService teamService) {
    this.teamMemberService = teamMemberService;
    this.teamMemberViewService = teamMemberViewService;
    this.controllerHelperService = controllerHelperService;
    this.regulatorTeamMemberEditRolesValidator = regulatorTeamMemberEditRolesValidator;
    this.teamMemberRoleService = teamMemberRoleService;
    this.teamService = teamService;
  }


  @GetMapping("/{wuaId}")
  public ModelAndView renderEditMember(@PathVariable("teamId") TeamId teamId,
                                       @PathVariable("wuaId") WebUserAccountId wuaId) {

    var form = new TeamMemberRolesForm();
    var team = teamService.getTeam(teamId);

    var teamMember = teamMemberService.getTeamMember(team, wuaId);
    var userView = teamMemberViewService.getTeamMemberView(teamMember);
    form.setRoles(TeamRoleUtil.getRoleNames(teamMember.roles()));

    return getEditModelAndView(teamId, userView, form);

  }

  @PostMapping("/{wuaId}")
  public ModelAndView editMember(@PathVariable("teamId") TeamId teamId,
                                 @PathVariable("wuaId") WebUserAccountId wuaId,
                                 @ModelAttribute("form") TeamMemberRolesForm form,
                                 BindingResult bindingResult) {

    var team = teamService.getTeam(teamId);

    var teamMember = teamMemberService.getTeamMember(team, wuaId);

    var userView = teamMemberViewService.getTeamMemberView(teamMember);

    regulatorTeamMemberEditRolesValidator.validate(form, bindingResult,
        new RegulatorTeamMemberEditRolesValidatorDto(team, teamMember));

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        getEditModelAndView(teamId, userView, form),
        form,
        () -> {
          teamMemberRoleService.updateUserTeamRoles(team, teamMember.wuaId().id(), form.getRoles());
          return ReverseRouter.redirect(on(RegulatorTeamMemberController.class).renderMemberList(teamId));
        });

  }

  private ModelAndView getEditModelAndView(TeamId teamId, TeamMemberView userView, TeamMemberRolesForm form) {
    return new ModelAndView("scap/permissionmanagement/AddTeamMemberRoles")
        .addObject("pageTitle", userView.getDisplayName())
        .addObject("form", form)
        .addObject("userDisplayName", userView.getDisplayName())
        .addObject("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(RegulatorTeamRole.class))
        .addObject("backLinkUrl",
          ReverseRouter.route(on(RegulatorTeamMemberController.class).renderMemberList(teamId))
        );
  }

}
