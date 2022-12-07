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
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.AddTeamMemberForm;
import uk.co.nstauthority.scap.permissionmanagement.AddTeamMemberValidator;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;

@Controller
@RequestMapping("/permission-management/regulator/{teamId}")
class RegulatorAddMemberController extends AbstractRegulatorPermissionManagement {

  private final CustomerConfigurationProperties customerConfigurationProperties;

  private final SamlProperties samlProperties;

  private final ControllerHelperService controllerHelperService;

  private final AddTeamMemberValidator addTeamMemberValidator;

  private final EnergyPortalUserService energyPortalUserService;

  @Autowired
  RegulatorAddMemberController(RegulatorTeamService regulatorTeamService,
                               CustomerConfigurationProperties customerConfigurationProperties,
                               SamlProperties samlProperties, ControllerHelperService controllerHelperService,
                               AddTeamMemberValidator addTeamMemberValidator,
                               EnergyPortalUserService energyPortalUserService) {
    super(regulatorTeamService);
    this.customerConfigurationProperties = customerConfigurationProperties;
    this.samlProperties = samlProperties;
    this.controllerHelperService = controllerHelperService;
    this.addTeamMemberValidator = addTeamMemberValidator;
    this.energyPortalUserService = energyPortalUserService;
  }

  @GetMapping("/add-member")
  ModelAndView renderAddTeamMember(@PathVariable("teamId") TeamId teamId) {
    getRegulatorTeam(teamId);
    return getAddTeamMemberModelAndView(teamId, new AddTeamMemberForm());
  }

  @PostMapping("/add-member")
  ModelAndView addMemberToTeamSubmission(@PathVariable("teamId") TeamId teamId,
                                         @ModelAttribute("form") AddTeamMemberForm form,
                                         BindingResult bindingResult) {

    getRegulatorTeam(teamId);
    addTeamMemberValidator.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        getAddTeamMemberModelAndView(teamId, form),
        form,
        () -> {
          var userToAdd = energyPortalUserService.findUsersByUsername(form.getUsername().getInputValue()).get(0);
          return ReverseRouter.redirect(on(RegulatorAddRolesController.class)
              .renderAddTeamMemberRoles(teamId, new WebUserAccountId(userToAdd.webUserAccountId())));
        }
    );
  }


  private ModelAndView getAddTeamMemberModelAndView(TeamId teamId, AddTeamMemberForm form) {
    return new ModelAndView("scap/permissionmanagement/regulator/regulatorAddTeamMember")
        .addObject("htmlTitle", "Add user to %s".formatted(customerConfigurationProperties.mnemonic()))
        .addObject("registrationUrl", samlProperties.getRegistrationUrl())
        .addObject("form", form)
        .addObject(
            "submitUrl",
            ReverseRouter.route(on(RegulatorAddMemberController.class)
                .addMemberToTeamSubmission(teamId, form, ReverseRouter.emptyBindingResult()))
        )
        .addObject(
            "backLinkUrl",
            ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId))
        );
  }
}