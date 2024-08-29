package uk.co.nstauthority.scap.scap.summary;


import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.RolePermission.SUBMIT_SCAP;
import static uk.co.nstauthority.scap.permissionmanagement.RolePermission.VIEW_SCAP;

import java.util.Collections;
import java.util.List;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventDocumentService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventView;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;

@Controller
@RequestMapping("{scapId}")
@HasAnyPermissionForScap(allowRegulatorAccess = true, permissions = {SUBMIT_SCAP, VIEW_SCAP})
public class ScapSummaryController {

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final CaseEventService caseEventService;

  private final OrganisationGroupService organisationGroupService;

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;

  private final UserDetailService userDetailService;

  private final CaseEventDocumentService caseEventDocumentService;

  private final UpdateRequestService updateRequestService;

  public ScapSummaryController(ScapDetailService scapDetailService,
                               ScapSummaryViewService scapSummaryViewService,
                               CaseEventService caseEventService,
                               OrganisationGroupService organisationGroupService,
                               TeamService teamService,
                               TeamMemberService teamMemberService, UserDetailService userDetailService,
                               UpdateRequestService updateRequestService,
                               CaseEventDocumentService caseEventDocumentService) {
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.caseEventService = caseEventService;
    this.organisationGroupService = organisationGroupService;
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
    this.updateRequestService = updateRequestService;
    this.caseEventDocumentService = caseEventDocumentService;
  }

  @GetMapping
  @ScapHasStatus(permittedStatuses = {ScapDetailStatus.DRAFT,
      ScapDetailStatus.SUBMITTED,
      ScapDetailStatus.APPROVED,
      ScapDetailStatus.CLOSED_OUT,
      ScapDetailStatus.WITHDRAWN})
  public ModelAndView getScapSummary(@PathVariable("scapId") ScapId scapId) {
    return getScapSummary(scapId, null);
  }

  @GetMapping(path = "/{versionNumber}")
  @ScapHasStatus(permittedStatuses = {ScapDetailStatus.DRAFT,
      ScapDetailStatus.SUBMITTED,
      ScapDetailStatus.APPROVED,
      ScapDetailStatus.CLOSED_OUT,
      ScapDetailStatus.WITHDRAWN})
  public ModelAndView getScapSummary(@PathVariable("scapId") ScapId scapId,
                                     @PathVariable("versionNumber") Integer versionNumber) {
    var versionedDetail = scapDetailService.getActionableScapDetail(scapId, userDetailService.getUserDetail());
    if (versionNumber != null && versionNumber < versionedDetail.getVersionNumber()) {
      versionedDetail = scapDetailService.getByScapIdAndVersionNumber(scapId, versionNumber);
    }

    var user = userDetailService.getUserDetail();
    var userPermissions = teamMemberService.getAllPermissionsForUser(user);
    if (ScapDetailStatus.DRAFT.equals(versionedDetail.getStatus())
        && versionedDetail.getVersionNumber() == 1
        && userPermissions.contains(SUBMIT_SCAP)) {
      return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
    }

    var userPermissionsBasedOnTeams = teamService.findAllPermissionsForUserInOrganisationGroup(
        user.wuaId(),
        versionedDetail.getScap().getOrganisationGroupId());

    var scapSummary = scapSummaryViewService.getScapSummaryView(versionedDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(versionedDetail.getScap().getOrganisationGroupId(), "Get Org Group for Summary");

    var generator = ScapSummaryModelAndViewGenerator.generator(
                versionedDetail,
                scapSummary,
                caseEventDocumentService)
        .withScapStatus(scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary))
        .withCaseEventTimeline(getCaseEventView(scapId))
        .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
        .withIsUpdateable(userPermissionsBasedOnTeams, versionedDetail.getStatus())
        .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId))
        .withScapVersions(scapDetailService.getAllVersionsForUser(versionedDetail.getScap()))
        .withCurrentVersion(versionedDetail.getVersionNumber());
    orgGroup.ifPresent(generator::withOrgGroup);
    updateRequestService.findNextDueUpdate(scapId)
        .ifPresent(requestEvent -> generator.withUpdateRequestText(requestEvent
            .getCaseEvent()
            .getComments()));

    return generator.generate();
  }

  @PostMapping("/")
  public ModelAndView getScapVersionSummary(@PathVariable("scapId") ScapId scapId,
                                            @ModelAttribute("versionSelectForm") VersionSelectForm form) {
    return ReverseRouter.redirect(on(ScapSummaryController.class)
        .getScapSummary(scapId, form.getRequestedVersion()))
        .addObject("versionSelectForm", form);
  }

  private List<CaseEventView> getCaseEventView(ScapId scapId) {
    if (teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail())) {
      return caseEventService.getEventViewByScapId(scapId);
    }
    return Collections.emptyList();
  }
}
