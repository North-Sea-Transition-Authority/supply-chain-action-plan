package uk.co.nstauthority.scap.workarea;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.RolePermission.SUBMIT_SCAP;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.SessionAttributes;
import org.springframework.web.bind.support.SessionStatus;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupRestController;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsRestController;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;
import uk.co.nstauthority.scap.scap.start.ScapStartController;

// Hide null warning when ReverseRouting with null parameters on WorkAreaController mappings
@SuppressWarnings("ConstantConditions")
@Controller
@RequestMapping("/work-area")
@SessionAttributes({"workAreaFilter"})
public class WorkAreaController {

  private final UserDetailService userDetailService;

  private final TeamMemberService teamMemberService;
  private final WorkAreaService workAreaService;
  private final TeamService teamService;
  private final WorkAreaFormService workAreaFormService;

  static final String FIELD_SEARCH_URL =
      ReverseRouter.route(on(ProjectDetailsRestController.class).getFieldSearchResults(null));

  @Autowired
  public WorkAreaController(UserDetailService userDetailService,
                            TeamMemberService teamMemberService,
                            WorkAreaService workAreaService,
                            TeamService teamService,
                            WorkAreaFormService workAreaFormService) {
    this.userDetailService = userDetailService;
    this.teamMemberService = teamMemberService;
    this.workAreaService = workAreaService;
    this.teamService = teamService;
    this.workAreaFormService = workAreaFormService;
  }

  @GetMapping
  public ModelAndView getWorkArea(@ModelAttribute("workAreaFilter") WorkAreaFilter filter) {
    var user = userDetailService.getUserDetail();
    var userPermissions = teamMemberService.getAllPermissionsForUser(user);
    var teams = teamService.getTeamsThatUserBelongsTo(user);
    var isRegulator = teams.stream()
        .anyMatch(team -> TeamType.REGULATOR.equals(team.getTeamType()));

    var workAreaItems = workAreaService.getWorkAreaItems(filter, isRegulator, teams);

    var form = WorkAreaForm.from(filter);
    var statusCheckboxes = ScapDetailStatus.getRadioOptions();
    var updateRequestRadios = UpdateRequestStatusRadioOptions.getRadioOptions();
    var prefilledOperator = workAreaFormService.getPreselectedOrganisation(form.getOperatorId());
    var prefilledField = workAreaFormService.getPreselectedField(form.getFieldId());

    return new ModelAndView("scap/workarea/workArea")
        .addObject("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap()))
        .addObject("canStartScap", userPermissions.contains(SUBMIT_SCAP))
        .addObject("workAreaItems", workAreaItems)
        .addObject("clearFiltersUrl",
            ReverseRouter.route(on(WorkAreaController.class).clearWorkAreaFilter(null, null)))
        .addObject("form", form)
        .addObject("statusCheckboxes", statusCheckboxes)
        .addObject("updateRequestRadios", updateRequestRadios)
        .addObject("isRegulator", isRegulator)
        .addObject("organisationGroupSearchUrl",
            ReverseRouter.route(on(OrganisationGroupRestController.class).getOrganisationGroupSearchResults(null)))
        .addObject("prefilledOperator", prefilledOperator)
        .addObject("fieldSearchUrl", FIELD_SEARCH_URL)
        .addObject("prefilledField", prefilledField)
        .addObject("projectTypeCheckboxes", ProjectType.getCheckboxItems());
  }

  @PostMapping
  ModelAndView filterWorkArea(@ModelAttribute("form") WorkAreaForm form,
                              @ModelAttribute("workAreaFilter") WorkAreaFilter filter) {
    filter.update(form);
    return ReverseRouter.redirect(on(WorkAreaController.class).getWorkArea(null));
  }

  @GetMapping("/clear-filters")
  public ModelAndView clearWorkAreaFilter(@ModelAttribute("workAreaFilter") WorkAreaFilter filter,
                                          SessionStatus sessionStatus) {
    sessionStatus.setComplete();
    filter.clearFilter();
    return ReverseRouter.redirect(on(WorkAreaController.class).getWorkArea(null));
  }

  @ModelAttribute("workAreaFilter")
  private WorkAreaFilter getDefaultFilter() {
    var defaultFilter = new WorkAreaFilter();
    defaultFilter.setScapStatuses(ScapDetailStatus.getDefaultStatuses());
    defaultFilter.setUpdateRequestStatusRadioOptions(UpdateRequestStatusRadioOptions.ALL);
    return defaultFilter;
  }
}
