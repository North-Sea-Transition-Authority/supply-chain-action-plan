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
import uk.co.nstauthority.scap.configuration.SamlProperties;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.AddTeamMemberForm;
import uk.co.nstauthority.scap.permissionmanagement.IsMemberOfTeamOrRegulator;
import uk.co.nstauthority.scap.permissionmanagement.PermissionsRequiredForTeam;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.teams.AddMemberController;
import uk.co.nstauthority.scap.permissionmanagement.teams.AddTeamMemberValidator;

@Controller
@IsMemberOfTeamOrRegulator
@RequestMapping("/permission-management/industry/{teamId}")
@PermissionsRequiredForTeam(permissions = RolePermission.MANAGE_ORGANISATIONS)
class IndustryAddMemberController extends AddMemberController {

  @Autowired
  IndustryAddMemberController(SamlProperties samlProperties,
                              ControllerHelperService controllerHelperService,
                              AddTeamMemberValidator addTeamMemberValidator,
                              EnergyPortalUserService energyPortalUserService) {
    super(samlProperties,
        controllerHelperService,
        addTeamMemberValidator,
        energyPortalUserService);
  }

  @GetMapping("/add-member")
  ModelAndView renderAddTeamMember(@PathVariable("teamId") TeamId teamId) {
    var form = new AddTeamMemberForm();
    return getAddTeamMemberModelAndView(form)
        .addObject(
            "submitUrl",
            ReverseRouter.route(on(IndustryAddMemberController.class)
                .addMemberToTeamSubmission(teamId, form, ReverseRouter.emptyBindingResult()))
        )
        .addObject(
            "backLinkUrl",
            ReverseRouter.route(on(IndustryTeamManagementController.class).renderMemberList(teamId))
        );
  }

  @PostMapping("/add-member")
  public ModelAndView addMemberToTeamSubmission(@PathVariable("teamId") TeamId teamId,
                                                @ModelAttribute("form") AddTeamMemberForm form,
                                                BindingResult bindingResult) {
    return super.addMemberToTeamSubmission(teamId,
        form,
        bindingResult,
        IndustryAddRolesController.class);
  }
}