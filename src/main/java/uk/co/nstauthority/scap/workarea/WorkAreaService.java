package uk.co.nstauthority.scap.workarea;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;

@Service
class WorkAreaService {

  static final String ORGANISATION_GROUPS_REQUEST_PURPOSE = "Get organisation groups to show on SCAP work area";

  private final WorkAreaItemDtoRepository workAreaItemDtoRepository;
  private final OrganisationGroupService organisationGroupService;
  private final UserDetailService userDetailService;
  private final TeamService teamService;

  private final CaseEventService caseEventService;

  @Autowired
  WorkAreaService(WorkAreaItemDtoRepository workAreaItemDtoRepository,
                  OrganisationGroupService organisationGroupService,
                  UserDetailService userDetailService,
                  TeamService teamService,
                  CaseEventService caseEventService) {
    this.workAreaItemDtoRepository = workAreaItemDtoRepository;
    this.organisationGroupService = organisationGroupService;
    this.userDetailService = userDetailService;
    this.teamService = teamService;
    this.caseEventService = caseEventService;
  }

  public List<WorkAreaItem> getWorkAreaItems() {
    var user = userDetailService.getUserDetail();
    var teams = teamService.getTeamsThatUserBelongsTo(user);
    var isRegulator = teams.stream()
        .anyMatch(team -> TeamType.REGULATOR.equals(team.getTeamType()));

    if (isRegulator) {
      return getRegulatorWorkAreaItems();
    }

    return getIndustryWorkAreaItems(teams);
  }

  private List<WorkAreaItem> getRegulatorWorkAreaItems() {
    var workAreaItemDtoList = workAreaItemDtoRepository.getAllByScapStatusNotIn(ScapDetailStatus.DRAFT);

    return getItemsFromDtoList(workAreaItemDtoList);
  }

  private List<WorkAreaItem> getIndustryWorkAreaItems(List<Team> teams) {
    var organisationGroupIds = teams.stream().map(Team::getEnergyPortalOrgGroupId).toList();
    if (organisationGroupIds.isEmpty()) {
      return Collections.emptyList();
    }
    var workAreaItemDtoList = workAreaItemDtoRepository.getAllByOrganisationGroups(organisationGroupIds);
    return getItemsFromDtoList(workAreaItemDtoList);
  }

  private List<WorkAreaItem> getItemsFromDtoList(List<WorkAreaItemDto> workAreaItemDtoList) {
    // This avoids an unnecessary API call
    if (workAreaItemDtoList.isEmpty()) {
      return Collections.emptyList();
    }

    var organisationGroupIds = workAreaItemDtoList.stream()
        .map(WorkAreaItemDto::organisationGroupId)
        .toList();
    var organisationGroups = organisationGroupService.getOrganisationGroupsByIds(
        organisationGroupIds, ORGANISATION_GROUPS_REQUEST_PURPOSE);
    var organisationGroupsMap = organisationGroups.stream()
        .collect(Collectors.toMap(OrganisationGroup::getOrganisationGroupId, OrganisationGroup::getName));
    return workAreaItemDtoList.stream()
        .sorted(Comparator
            .comparing((WorkAreaItemDto dto) -> dto.status().getDisplayOrder())
            .thenComparing(sortByItemDate(), Comparator.reverseOrder()))
        .map(workAreaItemDto -> new WorkAreaItem(
            new ScapId(workAreaItemDto.scapId()),
            workAreaItemDto.scapVersionNumber(),
            workAreaItemDto.reference(),
            organisationGroupsMap.getOrDefault(workAreaItemDto.organisationGroupId(), "MISSING OPERATOR"),
            workAreaItemDto.projectName(),
            workAreaItemDto.status(),
            inferStatus(workAreaItemDto),
            caseEventService.isFurtherInfoResponseOutstanding(new ScapId(workAreaItemDto.scapId()))))
        .toList();

  }

  @VisibleForTesting
  ScapSubmissionStage inferStatus(WorkAreaItemDto workAreaItemDto) {
    if (Boolean.TRUE.equals(workAreaItemDto.projectClosedOut())) {
      return ScapSubmissionStage.PROJECT_COMPLETED;
    } else if (Boolean.TRUE.equals(workAreaItemDto.hasContractingPerformance())) {
      return ScapSubmissionStage.CONTRACTING_PERFORMANCE;
    } else if (Boolean.TRUE.equals(workAreaItemDto.hasActualTender())) {
      return ScapSubmissionStage.ACTUAL_TENDER;
    } else if (Boolean.TRUE.equals(workAreaItemDto.hasPlannedTender())) {
      return ScapSubmissionStage.PLANNED_TENDER;
    } else if (Objects.nonNull(workAreaItemDto.projectName())) {
      return ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING;
    }
    return ScapSubmissionStage.DRAFT;
  }

  @SuppressWarnings("SwitchStatementWithTooFewBranches")
  private Function<WorkAreaItemDto, Instant> sortByItemDate() {
    return dto -> switch (dto.status()) {
      case SUBMITTED -> dto.submittedTimestamp();
      default -> dto.createdTimestamp();
    };
  }
}
