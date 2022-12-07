package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
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
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;

@Controller
@RequestMapping("/permission-management/regulator/{teamId}")
class RegulatorAddRolesController extends AbstractRegulatorPermissionManagement {

  private final EnergyPortalUserService energyPortalUserService;

  private final RegulatorTeamMemberRolesValidator regulatorTeamMemberRolesValidator;

  private final RegulatorTeamService regulatorTeamService;

  private final ControllerHelperService controllerHelperService;

  @Autowired
  RegulatorAddRolesController(RegulatorTeamService regulatorTeamService,
                              ControllerHelperService controllerHelperService,
                              EnergyPortalUserService energyPortalUserService,
                              RegulatorTeamMemberRolesValidator regulatorTeamMemberRolesValidator) {
    super(regulatorTeamService);
    this.regulatorTeamService = regulatorTeamService;
    this.energyPortalUserService = energyPortalUserService;
    this.regulatorTeamMemberRolesValidator = regulatorTeamMemberRolesValidator;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping("/add-member/{webUserAccountId}/roles")
  ModelAndView renderAddTeamMemberRoles(@PathVariable("teamId") TeamId teamId,
                                        @PathVariable("webUserAccountId") WebUserAccountId webUserAccountId) {
    getRegulatorTeam(teamId);
    var energyPortalUser = energyPortalUserService.getEnergyPortalUser(webUserAccountId);
    return getAddTeamMemberRolesModelAndView(teamId, energyPortalUser, new TeamMemberRolesForm());
  }

  @PostMapping("/add-member/{webUserAccountId}/roles")
  ModelAndView saveAddTeamMemberRoles(@PathVariable("teamId") TeamId teamId,
                                      @PathVariable("webUserAccountId") WebUserAccountId webUserAccountId,
                                      @ModelAttribute("form") TeamMemberRolesForm form,
                                      BindingResult bindingResult) {
    var team = getRegulatorTeam(teamId);
    var energyPortalUser = energyPortalUserService.getEnergyPortalUser(webUserAccountId);

    regulatorTeamMemberRolesValidator.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        getAddTeamMemberRolesModelAndView(teamId, energyPortalUser, form),
        form,
        () -> {
          var regulatorRoles = form.getRoles()
              .stream()
              .map(RegulatorTeamRole::getRoleFromString)
              .filter(Optional::isPresent)
              .map(Optional::get)
              .collect(Collectors.toSet());

          regulatorTeamService.addUserTeamRoles(team, energyPortalUser, regulatorRoles);
          return ReverseRouter.redirect(on(RegulatorTeamManagementController.class).renderMemberList(teamId));
        }
    );
  }

  private ModelAndView getAddTeamMemberRolesModelAndView(TeamId teamId,
                                                         EnergyPortalUserDto energyPortalUser,
                                                         TeamMemberRolesForm form) {
    return new ModelAndView("scap/permissionmanagement/regulator/regulatorTeamMemberRoles")
        .addObject("userDisplayName", energyPortalUser.displayName())
        .addObject("form", form)
        .addObject("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(RegulatorTeamRole.class))
        .addObject(
            "backLinkUrl",
            ReverseRouter.route(on(RegulatorAddMemberController.class).renderAddTeamMember(teamId))
        );
  }
}