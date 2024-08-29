package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Comparator;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.endpointvalidation.annotations.UserHasAnyPermission;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.restapi.scap.ScapRestController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.start.ScapStartController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.util.StreamUtils;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@Controller
@UserHasAnyPermission(permissions = RolePermission.SUBMIT_SCAP)
public class OrganisationGroupController {

  private final String newScapBackLinkUrl =
      ReverseRouter.route(on(ScapStartController.class).renderStartNewScap());

  private final ScapService scapService;
  private final OrganisationGroupFormService organisationGroupFormService;
  private final ValidationErrorOrderingService validationErrorOrderingService;
  private final ScapDetailService scapDetailService;
  private final UserDetailService userDetailService;
  private final TeamService teamService;
  private final ScapOperatorService scapOperatorService;

  @Autowired
  public OrganisationGroupController(ScapService scapService,
                                     OrganisationGroupFormService organisationGroupFormService,
                                     ValidationErrorOrderingService validationErrorOrderingService,
                                     ScapDetailService scapDetailService,
                                     UserDetailService userDetailService,
                                     TeamService teamService,
                                     ScapOperatorService scapOperatorService) {
    this.scapService = scapService;
    this.organisationGroupFormService = organisationGroupFormService;
    this.validationErrorOrderingService = validationErrorOrderingService;
    this.scapDetailService = scapDetailService;
    this.userDetailService = userDetailService;
    this.teamService = teamService;
    this.scapOperatorService = scapOperatorService;
  }

  @GetMapping("/new/organisation-group")
  public ModelAndView renderNewScapOrganisationGroupForm(@ModelAttribute("form") OrganisationGroupForm form) {
    return organisationGroupFormModelAndView(newScapBackLinkUrl);
  }

  @PostMapping("/new/organisation-group")
  public ModelAndView saveNewScapOrganisationGroup(@ModelAttribute("form") OrganisationGroupForm form,
                                                   BindingResult bindingResult) {
    bindingResult = organisationGroupFormService.validate(form, bindingResult);
    if (bindingResult.hasErrors()) {
      return organisationGroupFormModelAndView(newScapBackLinkUrl)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    var scap = scapOperatorService.createScap(form);
    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scap.getScapId()));
  }

  @GetMapping("/{scapId}/organisation-group")
  @ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
  public ModelAndView renderExistingScapOrganisationGroupForm(@PathVariable("scapId") ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    var form = organisationGroupFormService.getForm(scapDetail);
    var existingScapBackLinkUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId));

    return organisationGroupFormModelAndView(existingScapBackLinkUrl)
        .addObject("form", form)
        .addObject("preselectedScap", organisationGroupFormService.getPreselectedScap(scapDetail));
  }

  @PostMapping("/{scapId}/organisation-group")
  @ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
  public ModelAndView saveExistingScapOrganisationGroup(@ModelAttribute("form") OrganisationGroupForm form,
                                                        @PathVariable("scapId") ScapId scapId,
                                                        BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);
    bindingResult = organisationGroupFormService.validate(form, bindingResult);
    if (bindingResult.hasErrors()) {
      var existingScapBackLinkUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId));
      return organisationGroupFormModelAndView(existingScapBackLinkUrl)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult))
          .addObject("preselectedScap", organisationGroupFormService.getPreselectedScap(scapDetail, bindingResult));
    }

    scapOperatorService.updateScapOperator(scap, scapDetail, form);
    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
  }

  private ModelAndView organisationGroupFormModelAndView(String backLinkUrl) {
    var user = userDetailService.getUserDetail();
    var userTeams = teamService.findAllTeamsForUserBasedOnPermission(List.of(IndustryTeamRole.SCAP_SUBMITTER), user.wuaId());

    var permittedOrganisationGroups = userTeams.stream()
        .sorted(Comparator.comparing(Team::getDisplayName))
        .collect(StreamUtils.toLinkedHashMap(
            team -> String.valueOf(team.getEnergyPortalOrgGroupId()),
            Team::getDisplayName
        ));

    return new ModelAndView("scap/scap/organisationGroup")
        .addObject("backLinkUrl", backLinkUrl)
        .addObject("permittedOrganisationGroups", permittedOrganisationGroups)
        .addObject("scapSearchRestUrl",
            ReverseRouter.route(on(ScapRestController.class).searchScaps(null)));
  }
}
