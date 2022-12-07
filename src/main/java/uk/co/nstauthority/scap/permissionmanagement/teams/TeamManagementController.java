package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupRestController;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.util.NotificationBannerUtils;

@Controller
@RequestMapping("/permission-management")
public class TeamManagementController {

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  private final ControllerHelperService controllerHelperService;

  private final OrganisationGroupService organisationGroupService;

  private final NewTeamFormvalidator newTeamFormvalidator;

  private final String organisationGroupSearchRestUrl =
      ReverseRouter.route(on(OrganisationGroupRestController.class).getOrganisationGroupSearchResults(null));

  @Autowired
  public TeamManagementController(TeamService teamService, UserDetailService userDetailService,
                                  ControllerHelperService controllerHelperService,
                                  OrganisationGroupService organisationGroupService,
                                  NewTeamFormvalidator newTeamFormvalidator) {
    this.teamService = teamService;
    this.userDetailService = userDetailService;
    this.controllerHelperService = controllerHelperService;
    this.organisationGroupService = organisationGroupService;
    this.newTeamFormvalidator = newTeamFormvalidator;
  }

  @GetMapping
  public ModelAndView renderTeamList() {
    var teams = teamService.getTeamsThatUserBelongsTo(userDetailService.getUserDetail())
        .stream()
        .map(TeamView::fromTeam)
        .collect(Collectors.toList());

    return new ModelAndView("scap/permissionmanagement/teamList")
        .addObject("pageTitle", "Choose team to manage")
        .addObject("allTeams", teams)
        .addObject("newTeamFormUrl",
            ReverseRouter.route(on(TeamManagementController.class).renderNewIndustryTeamForm(null)));
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
          Integer.valueOf(groupId),
          userDetailService.getUserDetail().wuaId());

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
}
