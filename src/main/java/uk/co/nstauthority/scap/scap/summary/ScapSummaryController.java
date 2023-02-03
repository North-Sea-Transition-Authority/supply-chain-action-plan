package uk.co.nstauthority.scap.scap.summary;


import java.util.Collections;
import java.util.List;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventView;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Controller
@RequestMapping("{scapId}")
public class ScapSummaryController {

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final CaseEventService caseEventService;

  private final OrganisationGroupService organisationGroupService;

  private final TeamService teamService;

  private final UserDetailService userDetailService;

  public ScapSummaryController(ScapDetailService scapDetailService,
                               ScapSummaryViewService scapSummaryViewService,
                               CaseEventService caseEventService,
                               OrganisationGroupService organisationGroupService,
                               TeamService teamService,
                               UserDetailService userDetailService) {
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.caseEventService = caseEventService;
    this.organisationGroupService = organisationGroupService;
    this.teamService = teamService;
    this.userDetailService = userDetailService;
  }

  @GetMapping
  @ScapHasStatus(permittedStatuses = {ScapDetailStatus.DRAFT, ScapDetailStatus.SUBMITTED, ScapDetailStatus.APPROVED})
  public ModelAndView getScapSummary(@PathVariable("scapId") ScapId scapId) {
    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);

    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(), "Get Org Group for Summary");

    var generator =
        ScapSummaryModelAndViewGenerator.generator(scapDetail, scapSummary)
            .withScapStatus(scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary))
            .withCaseEventTimeline(getCaseEventView(scapId))
            .withApplicableActions(caseEventService.getApplicableActionsForScap(scapId));
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
