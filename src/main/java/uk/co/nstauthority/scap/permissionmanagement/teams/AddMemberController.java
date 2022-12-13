package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
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

  @Autowired
  protected AddMemberController(SamlProperties samlProperties,
                                ControllerHelperService controllerHelperService,
                                AddTeamMemberValidator addTeamMemberValidator,
                                EnergyPortalUserService energyPortalUserService) {
    this.samlProperties = samlProperties;
    this.controllerHelperService = controllerHelperService;
    this.addTeamMemberValidator = addTeamMemberValidator;
    this.energyPortalUserService = energyPortalUserService;
  }

  protected ModelAndView getAddTeamMemberModelAndView(AddTeamMemberForm form) {
    return new ModelAndView("scap/permissionmanagement/AddTeamMember")
        .addObject("registrationUrl", samlProperties.getRegistrationUrl())
        .addObject("form", form);
  }

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
          return ReverseRouter.redirect(on(controller)
              .renderAddTeamMemberRoles(teamId, new WebUserAccountId(userToAdd.webUserAccountId())));
        }
    );
  }
}