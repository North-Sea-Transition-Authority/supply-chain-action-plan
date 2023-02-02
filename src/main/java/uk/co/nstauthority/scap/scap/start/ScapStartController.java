package uk.co.nstauthority.scap.scap.start;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.RolePermission.SUBMIT_SCAP;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("/new")
public class ScapStartController {

  private final TeamMemberService teamMemberService;

  private final UserDetailService userDetailService;

  private final String workAreaUrl = ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null));
  private final String startRedirectUrl = ReverseRouter.route(on(OrganisationGroupController.class)
      .renderNewScapOrganisationGroupForm(null));

  public ScapStartController(TeamMemberService teamMemberService, UserDetailService userDetailService) {
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
  }

  @GetMapping
  public ModelAndView renderStartNewScap() {
    var user = userDetailService.getUserDetail();
    var userPermissions = teamMemberService.getAllPermissionsForUser(user);
    if (!(userPermissions.contains(SUBMIT_SCAP))) {
      return ReverseRouter.redirect(on(WorkAreaController.class).getWorkArea(null));
    }

    return new ModelAndView("scap/scap/start")
        .addObject("startScapRedirectUrl", startRedirectUrl)
        .addObject("backLinkUrl", workAreaUrl);
  }
}
