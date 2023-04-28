package uk.co.nstauthority.scap.permissionmanagement.industry;

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
import uk.co.nstauthority.scap.endpointvalidation.annotations.IsMemberOfTeam;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForTeam;
import uk.co.nstauthority.scap.permissionmanagement.teams.AddRolesController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;

@Controller
@IsMemberOfTeam(allowRegulatorAccess = true)
@RequestMapping("/permission-management/industry/{teamId}")
@PermissionsRequiredForTeam(permissions = RolePermission.MANAGE_ORGANISATIONS)
class IndustryAddRolesController extends AddRolesController {

  private final EnergyPortalUserService energyPortalUserService;

  private final IndustryTeamMemberRolesValidator industryTeamMemberRolesValidator;

  @Autowired
  protected IndustryAddRolesController(
      TeamService regulatorTeamService,
      ControllerHelperService controllerHelperService,
      EnergyPortalUserService energyPortalUserService,
      IndustryTeamMemberRolesValidator industryTeamMemberRolesValidator) {
    super(regulatorTeamService,
        controllerHelperService,
        energyPortalUserService);
    this.energyPortalUserService = energyPortalUserService;
    this.industryTeamMemberRolesValidator = industryTeamMemberRolesValidator;
  }

  @GetMapping("/add-member/{wuaId}/roles")
  public ModelAndView renderAddTeamMemberRoles(@PathVariable("teamId") TeamId teamId,
                                               @PathVariable("wuaId") WebUserAccountId webUserAccountId) {
    var energyPortalUser = energyPortalUserService.getEnergyPortalUser(webUserAccountId);
    return getAddTeamMemberRolesModelAndView(energyPortalUser, new TeamMemberRolesForm())
        .addObject("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(IndustryTeamRole.class))
        .addObject(
            "backLinkUrl",
            ReverseRouter.route(on(IndustryAddMemberController.class).renderAddTeamMember(teamId)))
        ;
  }

  @Override
  protected Set<? extends TeamRole> getRolesToAdd(Set<String> rolesToAdd) {
    return rolesToAdd
        .stream()
        .map(IndustryTeamRole::getRoleFromString)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.toSet());
  }

  @PostMapping("/add-member/{wuaId}/roles")
  protected ModelAndView saveAddTeamMemberRoles(@PathVariable("teamId") TeamId teamId,
                                                @PathVariable("wuaId") WebUserAccountId webUserAccountId,
                                                @ModelAttribute("form") TeamMemberRolesForm form,
                                                BindingResult bindingResult) {
    industryTeamMemberRolesValidator.validate(form, bindingResult);
    return super.saveAddTeamMemberRoles(teamId,
        webUserAccountId,
        form,
        bindingResult,
        ReverseRouter.redirect(on(IndustryTeamMemberController.class).renderMemberList(teamId)),
        ReverseRouter.redirect(on(IndustryAddRolesController.class).renderAddTeamMemberRoles(teamId, webUserAccountId)));
  }
}
