package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;

@Controller
@RequestMapping("/permission-management/regulator/{teamId}")
public abstract class AddRolesController {

  private final EnergyPortalUserService energyPortalUserService;

  private final TeamService teamService;

  private final ControllerHelperService controllerHelperService;

  @Autowired
  protected AddRolesController(TeamService regulatorTeamService,
                               ControllerHelperService controllerHelperService,
                               EnergyPortalUserService energyPortalUserService) {
    this.teamService = regulatorTeamService;
    this.energyPortalUserService = energyPortalUserService;
    this.controllerHelperService = controllerHelperService;
  }

  protected abstract ModelAndView renderAddTeamMemberRoles(TeamId teamId, WebUserAccountId webUserAccountId);

  protected abstract Set<? extends TeamRole> getRolesToAdd(Set<String> rolesToAdd);

  protected ModelAndView saveAddTeamMemberRoles(@PathVariable("teamId") TeamId teamId,
                                                @PathVariable("webUserAccountId") WebUserAccountId webUserAccountId,
                                                @ModelAttribute("form") TeamMemberRolesForm form,
                                                BindingResult bindingResult,
                                                ModelAndView successRedirect,
                                                ModelAndView failureRedirect) {
    var team = teamService.getTeam(teamId);
    var energyPortalUser = energyPortalUserService.getEnergyPortalUser(webUserAccountId);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        failureRedirect,
        form,
        () -> {
          var regulatorRoles = getRolesToAdd(form.getRoles());
          teamService.addUserTeamRoles(team, energyPortalUser, regulatorRoles);
          return successRedirect;
        }
    );
  }

  protected ModelAndView getAddTeamMemberRolesModelAndView(EnergyPortalUserDto energyPortalUser,
                                                           TeamMemberRolesForm form) {
    return new ModelAndView("scap/permissionmanagement/teamMemberRoles")
        .addObject("userDisplayName", energyPortalUser.displayName())
        .addObject("form", form);
  }
}