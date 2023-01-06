package uk.co.nstauthority.scap.workarea;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.RolePermission.SUBMIT_SCAP;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.start.ScapStartController;

@Controller
@RequestMapping("/work-area")
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
  public ModelAndView getWorkArea() {
    var user = userDetailService.getUserDetail();
    var userPermissions = teamMemberService.getAllPermissionsForUser(user);
    var workAreaItems = workAreaService.getWorkAreaItems();

    return new ModelAndView("scap/workarea/workArea")
        .addObject("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap()))
        .addObject("canStartScap", userPermissions.contains(SUBMIT_SCAP))
        .addObject("workAreaItems", workAreaItems);
  }
}
