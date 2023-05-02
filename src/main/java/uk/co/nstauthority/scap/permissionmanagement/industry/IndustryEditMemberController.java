package uk.co.nstauthority.scap.permissionmanagement.industry;

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
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForTeam;
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
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;


@Controller
@IsMemberOfTeam(allowRegulatorAccess = true)
@RequestMapping("/permission-management/industry/{teamId}/edit/{wuaId}")
@HasAnyPermissionForTeam(allowRegulatorAccess = true, permissions = RolePermission.MANAGE_ORGANISATIONS)
public class IndustryEditMemberController {
  private final TeamMemberService teamMemberService;
  private final TeamMemberViewService teamMemberViewService;

  private final TeamMemberRoleService teamMemberRoleService;
  private final ControllerHelperService controllerHelperService;

  private final IndustryTeamMemberEditRolesValidator industryTeamMemberEditRolesValidator;

  private final TeamService teamService;

  @Autowired
  IndustryEditMemberController(
      TeamMemberService teamMemberService,
      TeamMemberViewService teamMemberViewService,
      TeamMemberRoleService teamMemberRoleService,
      ControllerHelperService controllerHelperService,
      IndustryTeamMemberEditRolesValidator industryTeamMemberEditRolesValidator,
      TeamService teamService) {
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.teamMemberViewService = teamMemberViewService;
    this.teamMemberRoleService = teamMemberRoleService;
    this.controllerHelperService = controllerHelperService;
    this.industryTeamMemberEditRolesValidator = industryTeamMemberEditRolesValidator;
  }


  @GetMapping
  public ModelAndView renderEditMember(@PathVariable("teamId") TeamId teamId,
                                       @PathVariable("wuaId") WebUserAccountId wuaId) {

    var form = new TeamMemberRolesForm();
    var team = teamService.getTeam(teamId);
    var teamMember = teamMemberService.getTeamMember(team, wuaId);
    var userView = teamMemberViewService.getTeamMemberView(teamMember);

    form.setRoles(TeamRoleUtil.getRoleNames(teamMember.roles()));
    return getEditModelAndView(teamId, userView, form);

  }

  @PostMapping
  public ModelAndView editMember(@PathVariable("teamId") TeamId teamId,
                                 @PathVariable("wuaId") WebUserAccountId wuaId,
                                 @ModelAttribute("form") TeamMemberRolesForm form,
                                 BindingResult bindingResult) {

    var team = teamService.getTeam(teamId);
    var teamMember = teamMemberService.getTeamMember(team, wuaId);
    var userView = teamMemberViewService.getTeamMemberView(teamMember);

    industryTeamMemberEditRolesValidator.validate(form, bindingResult,
        new IndustryTeamMemberEditRolesValidatorDto(team, teamMember));

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        getEditModelAndView(teamId, userView, form),
        form,
        () -> {
          teamMemberRoleService.updateUserTeamRoles(team, teamMember.wuaId().id(), form.getRoles());
          return ReverseRouter.redirect(on(IndustryTeamMemberController.class).renderMemberList(teamId));
        });

  }

  private ModelAndView getEditModelAndView(TeamId teamId, TeamMemberView userView, TeamMemberRolesForm form) {
    return new ModelAndView("scap/permissionmanagement/AddTeamMemberRoles")
        .addObject("form", form)
        .addObject("pageTitle", userView.getDisplayName())
        .addObject("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(IndustryTeamRole.class))
        .addObject("backLinkUrl",
          ReverseRouter.route(on(IndustryTeamMemberController.class).renderMemberList(teamId))
        );
  }
}
