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
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberRolesForm;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberView;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamRoleUtil;

@Controller
@RequestMapping("/permission-management/regulator/{teamId}/edit")
public class RegulatorEditMemberController extends AbstractRegulatorPermissionManagement {
  private final TeamMemberService teamMemberService;
  private final TeamMemberViewService teamMemberViewService;
  private final RegulatorTeamMemberEditService regulatorTeamMemberEditService;
  private final ControllerHelperService controllerHelperService;
  private final RegulatorTeamMemberEditRolesValidator regulatorTeamMemberEditRolesValidator;

  @Autowired
  RegulatorEditMemberController(
      RegulatorTeamService regulatorTeamService,
      TeamMemberService teamMemberService,
      TeamMemberViewService teamMemberViewService,
      RegulatorTeamMemberEditService regulatorTeamMemberEditService,
      ControllerHelperService controllerHelperService,
      RegulatorTeamMemberEditRolesValidator regulatorTeamMemberEditRolesValidator) {
    super(regulatorTeamService);
    this.teamMemberService = teamMemberService;
    this.teamMemberViewService = teamMemberViewService;
    this.regulatorTeamMemberEditService = regulatorTeamMemberEditService;
    this.controllerHelperService = controllerHelperService;
    this.regulatorTeamMemberEditRolesValidator = regulatorTeamMemberEditRolesValidator;
  }


  @GetMapping("/{wuaId}")
  public ModelAndView renderEditMember(@PathVariable("teamId") TeamId teamId,
                                       @PathVariable("wuaId") WebUserAccountId wuaId) {

    var form = new TeamMemberRolesForm();
    var team = getRegulatorTeam(teamId);

    var teamMember = teamMemberService.getTeamMemberOrThrow(team, wuaId);
    var userView = teamMemberViewService.getTeamMemberViewOrThrow(teamMember);
    form.setRoles(TeamRoleUtil.getRoleNames(teamMember.roles()));

    return getEditModelAndView(teamId, userView, form);

  }

  @PostMapping("/{wuaId}")
  public ModelAndView editMember(@PathVariable("teamId") TeamId teamId,
                                 @PathVariable("wuaId") WebUserAccountId wuaId,
                                 @ModelAttribute("form") TeamMemberRolesForm form,
                                 BindingResult bindingResult) {

    var team = getRegulatorTeam(teamId);

    var teamMember = teamMemberService.getTeamMember(team, wuaId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "No user [%s] in team [%s]".formatted(wuaId, teamId)));

    var userView = teamMemberViewService.getTeamMemberViewOrThrow(teamMember);

    regulatorTeamMemberEditRolesValidator.validate(form, bindingResult,
        new RegulatorTeamMemberEditRolesValidatorDto(team, teamMember));

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        getEditModelAndView(teamId, userView, form),
        form,
        () -> {
          regulatorTeamMemberEditService.updateRoles(team, teamMember, form.getRoles());
          return ReverseRouter.redirect(on(RegulatorTeamManagementController.class).renderMemberList(teamId));
        });

  }

  private ModelAndView getEditModelAndView(TeamId teamId, TeamMemberView userView, TeamMemberRolesForm form) {
    return new ModelAndView("scap/permissionmanagement/regulator/regulatorTeamMemberRoles")
        .addObject("form", form)
        .addObject("userDisplayName", userView.getDisplayName())
        .addObject("roles", DisplayableEnumOptionUtil.getDisplayableOptionsWithDescription(RegulatorTeamRole.class))
        .addObject("backLinkUrl",
          ReverseRouter.route(on(RegulatorTeamManagementController.class).renderMemberList(teamId))
        );
  }

}
