package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.IsMemberOfTeam;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;


@Controller
@RequestMapping("/permission-management/industry")
public class IndustryTeamManagementController {

  private final TeamMemberViewService teamMemberViewService;

  private final IndustryTeamService industryTeamService;
  private final UserDetailService userDetailService;

  @Autowired
  IndustryTeamManagementController(TeamMemberViewService teamMemberViewService,
                                   IndustryTeamService industryTeamService, UserDetailService userDetailService) {
    this.teamMemberViewService = teamMemberViewService;
    this.industryTeamService = industryTeamService;
    this.userDetailService = userDetailService;
  }

  @GetMapping("/{teamId}")
  @IsMemberOfTeam
  public ModelAndView renderMemberList(@PathVariable("teamId") TeamId teamId) {

    var team = industryTeamService.getTeam(teamId);
    var user = userDetailService.getUserDetail();
    var modelAndView = new ModelAndView("scap/permissionmanagement/teamMembers")
        .addObject("pageTitle", "Manage %s".formatted(team.getDisplayName()))
        .addObject("teamName", team.getDisplayName())
        .addObject("teamRoles", IndustryTeamRole.values())
        .addObject("teamMembers", teamMemberViewService.getTeamMemberViewsForTeam(team));

    if (industryTeamService.isAccessManager(teamId, userDetailService.getUserDetail())) {
      modelAndView
          .addObject("addTeamMemberUrl",
              ReverseRouter.route(on(IndustryAddMemberController.class).renderAddTeamMember(teamId)))
          .addObject("canRemoveUsers", industryTeamService.isAccessManager(teamId, user))
          .addObject("canEditUsers", industryTeamService.isAccessManager(teamId, user));
    }
    return modelAndView;
  }
}