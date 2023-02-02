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
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
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

  @Autowired
  public WorkAreaController(UserDetailService userDetailService,
                            TeamMemberService teamMemberService,
                            WorkAreaService workAreaService) {
    this.userDetailService = userDetailService;
    this.teamMemberService = teamMemberService;
    this.workAreaService = workAreaService;
  }

  @GetMapping
  public ModelAndView getWorkArea(@ModelAttribute("workAreaFilter") WorkAreaFilter filter) {
    var user = userDetailService.getUserDetail();
    var userPermissions = teamMemberService.getAllPermissionsForUser(user);
    var workAreaItems = workAreaService.getWorkAreaItems(user, filter);
    var form = WorkAreaForm.from(filter);
    var statusCheckboxes = ScapDetailStatus.getRadioOptions();

    return new ModelAndView("scap/workarea/workArea")
        .addObject("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap()))
        .addObject("canStartScap", userPermissions.contains(SUBMIT_SCAP))
        .addObject("workAreaItems", workAreaItems)
        .addObject("clearFiltersUrl",
            ReverseRouter.route(on(WorkAreaController.class).clearWorkAreaFilter(null, null)))
        .addObject("form", form)
        .addObject("statusCheckboxes", statusCheckboxes);
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
    return new WorkAreaFilter();
  }
}
