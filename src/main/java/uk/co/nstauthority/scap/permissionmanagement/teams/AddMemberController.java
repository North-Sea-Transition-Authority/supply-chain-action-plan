package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.EnergyPortalAccessService;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.InstigatingWebUserAccountId;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.ResourceType;
import uk.co.fivium.digital.energyportalteamaccesslibrary.team.TargetWebUserAccountId;
import uk.co.fivium.energyportalapi.generated.client.UserProjectionRoot;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.AddTeamMemberForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;

@Controller
@RequestMapping("/permission-management/regulator/{teamId}")
public abstract class AddMemberController {

  private final SamlProperties samlProperties;

  private final ControllerHelperService controllerHelperService;

  private final AddTeamMemberValidator addTeamMemberValidator;

  private final EnergyPortalUserService energyPortalUserService;

  private final EnergyPortalAccessService energyPortalAccessService;

  private final UserDetailService userDetailService;

  private final TeamMemberService teamMemberService;

  @Autowired
  protected AddMemberController(SamlProperties samlProperties,
                                ControllerHelperService controllerHelperService,
                                AddTeamMemberValidator addTeamMemberValidator,
                                EnergyPortalUserService energyPortalUserService,
                                EnergyPortalAccessService energyPortalAccessService,
                                UserDetailService userDetailService,
                                TeamMemberService teamMemberService) {
    this.samlProperties = samlProperties;
    this.controllerHelperService = controllerHelperService;
    this.addTeamMemberValidator = addTeamMemberValidator;
    this.energyPortalUserService = energyPortalUserService;
    this.energyPortalAccessService = energyPortalAccessService;
    this.userDetailService = userDetailService;
    this.teamMemberService = teamMemberService;
  }

  protected ModelAndView getAddTeamMemberModelAndView(AddTeamMemberForm form) {
    return new ModelAndView("scap/permissionmanagement/AddTeamMember")
        .addObject("registrationUrl", samlProperties.getRegistrationUrl())
        .addObject("form", form);
  }

  @Transactional
  public ModelAndView addMemberToTeamSubmission(@PathVariable("teamId") TeamId teamId,
                                                @ModelAttribute("form") AddTeamMemberForm form,
                                                BindingResult bindingResult,
                                                Class<? extends AddRolesController> controller) {

    addTeamMemberValidator.validate(form, bindingResult);
    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        getAddTeamMemberModelAndView(form),
        form,
        () -> {
          var userToAdd = energyPortalUserService.findUsersByUsername(form.getUsername().getInputValue()).get(0);
          requestEnergyPortalAccess(userToAdd);
          return ReverseRouter.redirect(on(controller)
              .renderAddTeamMemberRoles(teamId, new WebUserAccountId(userToAdd.webUserAccountId())));
        }
    );
  }

  private void  requestEnergyPortalAccess(EnergyPortalUserDto userToAdd) {
    var loggedInUser = userDetailService.getUserDetail();

    //A user who already has roles in the system should not need to have the scap access role added again.
    if (teamMemberService.getAllPermissionsForUser(userToAdd.webUserAccountId()).isEmpty()) {
      energyPortalAccessService.addUserToAccessTeam(
          new ResourceType("SCAP_ACCESS_TEAM"),
          new TargetWebUserAccountId(userToAdd.webUserAccountId()),
          new InstigatingWebUserAccountId(loggedInUser.wuaId()));
    }
  }
}