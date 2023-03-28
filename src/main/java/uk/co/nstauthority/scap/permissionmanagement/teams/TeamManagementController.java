package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.UserHasAnyPermission;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupRestController;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.util.DeletionSuccessBannerUtil;
import uk.co.nstauthority.scap.util.NotificationBannerUtils;

@Controller
@RequestMapping("/permission-management")
@UserHasAnyPermission(permissions = RolePermission.MANAGE_ORGANISATIONS)
public class TeamManagementController {

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;

  private final UserDetailService userDetailService;

  private final ControllerHelperService controllerHelperService;

  private final OrganisationGroupService organisationGroupService;

  private final NewTeamFormvalidator newTeamFormvalidator;
  private final String organisationGroupSearchRestUrl =
      ReverseRouter.route(on(OrganisationGroupRestController.class).getOrganisationGroupSearchResults(null));

  @Autowired
  public TeamManagementController(TeamService teamService,
                                  TeamMemberService teamMemberService,
                                  UserDetailService userDetailService,
                                  ControllerHelperService controllerHelperService,
                                  OrganisationGroupService organisationGroupService,
                                  NewTeamFormvalidator newTeamFormvalidator) {
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
    this.controllerHelperService = controllerHelperService;
    this.organisationGroupService = organisationGroupService;
    this.newTeamFormvalidator = newTeamFormvalidator;
  }

  @GetMapping
  public ModelAndView renderTeamList() {
    var teamId = new TeamId(teamService.getRegulatorTeam().getUuid());
    var hasCreateTeamPermission = teamMemberService.isMemberOfTeamWithAnyRoleOf(teamId,
        userDetailService.getUserDetail(),
        Collections.singleton(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name()));

    var teams = teamService.findTeamsByUser(userDetailService.getUserDetail());
    if (hasCreateTeamPermission || teams.size() != 1) {
      return new ModelAndView("scap/permissionmanagement/teamList")
          .addObject("pageTitle", "Choose team to manage")
          .addObject("allTeams", teams)
          .addObject("hasCreateTeamPermissions", hasCreateTeamPermission)
          .addObject("newTeamFormUrl",
              ReverseRouter.route(on(TeamManagementController.class).renderNewIndustryTeamForm(null)));
    }
    var team = teams.get(0);
    if (team.teamType().equals(TeamType.REGULATOR)) {
      return ReverseRouter.redirect(on(RegulatorTeamManagementController.class).renderMemberList(team.teamId()));
    } else {
      return ReverseRouter.redirect(on(IndustryTeamManagementController.class).renderMemberList(team.teamId()));
    }
  }

  @GetMapping("/delete/{teamId}")
  public ModelAndView renderArchiveTeamConfirmation(@PathVariable("teamId") TeamId teamId) {
    return new ModelAndView("scap/permissionmanagement/removeTeam")
        .addObject("pageTitle", "Are you sure you want to archive this team?")
        .addObject("team", teamService.getTeam(teamId))
        .addObject("removeUrl",
            ReverseRouter.route(on(TeamManagementController.class).archiveTeam(teamId, null)))
        .addObject("backLinkUrl",
            ReverseRouter.route(on(IndustryTeamManagementController.class).renderMemberList(teamId)));
  }

  @PostMapping("/delete/{teamId}")
  public ModelAndView archiveTeam(@PathVariable("teamId") TeamId teamId,
                                  RedirectAttributes redirectAttributes) {
    var team = teamService.getTeam(teamId);
    teamService.archiveTeam(team);

    var successMessage = "%s has been successfully deleted".formatted(team.getDisplayName());
    DeletionSuccessBannerUtil.addRedirectionNotification(redirectAttributes, successMessage);

    return ReverseRouter.redirect(on(TeamManagementController.class).renderTeamList());
  }

  @GetMapping("/new-team")
  public ModelAndView renderNewIndustryTeamForm(@ModelAttribute("form") NewTeamForm form) {
    return new ModelAndView("scap/permissionmanagement/addTeam")
        .addObject("organisationGroupSearchRestUrl", organisationGroupSearchRestUrl)
        .addObject("submitFormUrl",
            ReverseRouter.route(on(TeamManagementController.class).addNewIndustryTeam(null, null)))
        .addObject("pageTitle", "Add new industry team");
  }

  @PostMapping("/new-team")
  public ModelAndView addNewIndustryTeam(@ModelAttribute("form") NewTeamForm form,
                                         BindingResult bindingResult) {

    newTeamFormvalidator.validate(form, bindingResult);
    return controllerHelperService.checkErrorsAndRedirect(bindingResult,
        renderNewIndustryTeamForm(form),
        form,
        () -> saveNewIndustryTeam(form));
  }

  private ModelAndView saveNewIndustryTeam(NewTeamForm form) {
    var groupId = form.getOrganisationGroupId().getInputValue();
    var orgGroup =
        organisationGroupService.getOrganisationGroupById(Integer.valueOf(groupId), "To Add Group to SCAP teams");

    if (orgGroup.isPresent()) {
      teamService.createTeam(orgGroup.get().getName(),
          Integer.valueOf(groupId));

      var view = renderTeamList();
      NotificationBannerUtils.successBanner(
          "Successfully added new organisation group team",
          new NotificationBannerBodyLine(
              "Team for organisation group %s has been created".formatted(orgGroup.get().getName()),
              "govuk-!-font-weight-bold"),
          view
      );
      return view;
    }
    return renderNewIndustryTeamForm(form);
  }

  private List<TeamView> getTeamList() {
    List<Team> teams;
    var user = userDetailService.getUserDetail();
    var regulatorTeams = teamService.getTeamsOfTypeThatUserBelongsTo(user, TeamType.REGULATOR);

    if (!(regulatorTeams.isEmpty())) {
      var regTeamMember = teamMemberService.getTeamMember(regulatorTeams.get(0), user.getWebUserAccountId());
      if (regTeamMember.roles().contains(RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER)) {
        teams = StreamSupport.stream(teamService.getAllTeams().spliterator(), false)
            .collect(Collectors.toList());
      } else {
        teams = teamService.getTeamsThatUserBelongsTo(user);
      }
    } else {
      teams = teamService.getTeamsThatUserBelongsTo(user);
    }

    return teams
        .stream()
        .map(TeamView::fromTeam)
        .collect(Collectors.toList());
  }
}
