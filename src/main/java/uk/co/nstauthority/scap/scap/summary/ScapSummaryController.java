package uk.co.nstauthority.scap.scap.summary;


import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventView;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}")
public class ScapSummaryController {

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final CaseEventService caseEventService;

  private final OrganisationGroupService organisationGroupService;

  private final TeamService teamService;

  private final TeamMemberService teamMemberService;

  private final UserDetailService userDetailService;

  private final SupportingDocumentService supportingDocumentService;

  public ScapSummaryController(ScapDetailService scapDetailService,
                               ScapSummaryViewService scapSummaryViewService,
                               CaseEventService caseEventService,
                               OrganisationGroupService organisationGroupService,
                               TeamService teamService,
                               TeamMemberService teamMemberService, UserDetailService userDetailService,
                               SupportingDocumentService supportingDocumentService) {
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.caseEventService = caseEventService;
    this.organisationGroupService = organisationGroupService;
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
    this.supportingDocumentService = supportingDocumentService;
  }

  //TODO:SCAP2022-232 - Smoke test all statuses against this method
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
    var scapDetail = scapDetailService.getActionableScapDetail(scapId, userDetailService.getUserDetail());
    ScapDetail versionedDetail = null;
    if (versionNumber != null) {
      versionedDetail = scapDetailService.getByScapIdAndVersionNumber(scapId, versionNumber);
    }

    var user = userDetailService.getUserDetail();
    var userPermissions = teamMemberService.getAllPermissionsForUser(user);
    if (ScapDetailStatus.DRAFT.equals(scapDetail.getStatus())
        && scapDetail.getVersionNumber() == 1
        && userPermissions.contains(RolePermission.SUBMIT_SCAP)) {
      return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
    }
    var scapSummary = scapSummaryViewService.getScapSummaryView(versionedDetail != null ? versionedDetail : scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(), "Get Org Group for Summary");

    var generator = ScapSummaryModelAndViewGenerator.generator(
                scapDetail,
                scapSummary,
                supportingDocumentService)
        .withScapStatus(scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary))
        .withCaseEventTimeline(getCaseEventView(scapId))
        .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId))
        .withUpdatePermission(teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail()))
        .withUpdateInProgress(scapDetailService.isUpdateInProgress(scapId))
        .withScapVersions(scapDetailService.getAllVersionsForUser(scapDetail.getScap()));
    orgGroup.ifPresent(generator::withOrgGroup);
    return generator.generate();
  }

  private List<CaseEventView> getCaseEventView(ScapId scapId) {
    if (teamService.userIsMemberOfRegulatorTeam(userDetailService.getUserDetail())) {
      return caseEventService.getEventViewByScapId(scapId);
    }
    return Collections.emptyList();
  }
}
