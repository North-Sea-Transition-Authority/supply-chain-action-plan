package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
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
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.AddRolesController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Controller
@IsMemberOfTeam
@RequestMapping("/permission-management/regulator/{teamId}")
@HasAnyPermissionForTeam(permissions = RolePermission.GRANT_ROLES)
class RegulatorAddRolesController extends AddRolesController {

  private final EnergyPortalUserService energyPortalUserService;

  private final RegulatorTeamMemberRolesValidator regulatorTeamMemberRolesValidator;

  @Autowired
  RegulatorAddRolesController(TeamService teamService,
                              ControllerHelperService controllerHelperService,
                              EnergyPortalUserService energyPortalUserService,
                              RegulatorTeamMemberRolesValidator regulatorTeamMemberRolesValidator) {
    super(teamService,
        controllerHelperService,
        energyPortalUserService);
    this.energyPortalUserService = energyPortalUserService;
    this.regulatorTeamMemberRolesValidator = regulatorTeamMemberRolesValidator;
  }

  @GetMapping("/add-member/{webUserAccountId}/roles")
  public ModelAndView renderAddTeamMemberRoles(@PathVariable("teamId") TeamId teamId,
                                               @PathVariable("webUserAccountId") WebUserAccountId webUserAccountId) {
    var energyPortalUser = energyPortalUserService.getEnergyPortalUser(webUserAccountId);
    return getAddTeamMemberRolesModelAndView(energyPortalUser, new TeamMemberRolesForm())
        .addObject("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(RegulatorTeamRole.class))
        .addObject(
            "backLinkUrl",
            ReverseRouter.route(on(RegulatorAddMemberController.class).renderAddTeamMember(teamId)));
  }

  @Override
  protected Set<? extends TeamRole> getRolesToAdd(Set<String> rolesToAdd) {
    return rolesToAdd
        .stream()
        .map(RegulatorTeamRole::getRoleFromString)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toSet());
  }

  @PostMapping("/add-member/{webUserAccountId}/roles")
  protected ModelAndView saveAddTeamMemberRoles(@PathVariable("teamId") TeamId teamId,
                                      @PathVariable("webUserAccountId") WebUserAccountId webUserAccountId,
                                      @ModelAttribute("form") TeamMemberRolesForm form,
                                      BindingResult bindingResult) {
    regulatorTeamMemberRolesValidator.validate(form, bindingResult);
    return super.saveAddTeamMemberRoles(teamId,
        webUserAccountId,
        form,
        bindingResult,
        ReverseRouter.redirect(on(RegulatorTeamMemberController.class).renderMemberList(teamId)),
        ReverseRouter.redirect(on(RegulatorAddRolesController.class).renderAddTeamMemberRoles(teamId, webUserAccountId)));
  }
}
