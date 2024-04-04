package uk.co.nstauthority.scap.workarea;

import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAPS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.jooq.Condition;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

@Service
class WorkAreaService {

  static final String ORGANISATION_GROUPS_REQUEST_PURPOSE = "Get organisation groups to show on SCAP work area";

  private final WorkAreaItemDtoRepository workAreaItemDtoRepository;
  private final OrganisationGroupService organisationGroupService;
  private final WorkAreaFilterService workAreaFilterService;
  private final TeamMemberService teamMemberService;
  private final UserDetailService userDetailService;

  @Autowired
  WorkAreaService(WorkAreaItemDtoRepository workAreaItemDtoRepository,
                  OrganisationGroupService organisationGroupService,
                  WorkAreaFilterService workAreaFilterService,
                  TeamMemberService teamMemberService,
                  UserDetailService userDetailService) {
    this.workAreaItemDtoRepository = workAreaItemDtoRepository;
    this.organisationGroupService = organisationGroupService;
    this.workAreaFilterService = workAreaFilterService;
    this.teamMemberService = teamMemberService;
    this.userDetailService = userDetailService;
  }

  public List<WorkAreaItem> getWorkAreaItems(WorkAreaFilter filter,
                                             boolean isRegulator,
                                             List<Team> teams) {
    var conditions = workAreaFilterService.getConditions(filter);


    if (isRegulator) {
      return getRegulatorWorkAreaItems(conditions);
    }

    var wuaId = userDetailService.getUserDetail().getWebUserAccountId();
    var teamsMemberRoles = teamMemberService.findAllRolesByUser(teams, wuaId);

    var teamsBasedOnSubmitterRole = findAllTeamsBasedOnRole(IndustryTeamRole.SCAP_SUBMITTER, teamsMemberRoles);
    var submitterItems = getIndustryWorkAreaItems(teamsBasedOnSubmitterRole, conditions)
        .stream();

    var teamsBasedOnViewerRole = findAllTeamsBasedOnRole(IndustryTeamRole.SCAP_VIEWER, teamsMemberRoles);
    var viewerItems = getIndustryWorkAreaItems(teamsBasedOnViewerRole, conditions)
        .stream()
        .filter(workAreaItem -> !workAreaItem.status().equals(ScapDetailStatus.DRAFT));

    return Stream.concat(submitterItems, viewerItems)
        .distinct()
        .sorted(Comparator
            .comparing((WorkAreaItem dto) -> dto.status().getDisplayOrder()).reversed())
        .toList();
  }

  private List<WorkAreaItem> getRegulatorWorkAreaItems(ArrayList<Condition> conditions) {
    var workAreaItemDtoList = workAreaItemDtoRepository.performQuery(
        conditions,
        SCAP_DETAILS.STATUS.notEqual(ScapDetailStatus.DRAFT.getEnumName()));
    return getItemsFromDtoList(workAreaItemDtoList);
  }

  private List<Team> findAllTeamsBasedOnRole(IndustryTeamRole industryTeamRole, List<TeamMemberRole> teamMemberRoles) {
    return teamMemberRoles.stream()
        .filter(teamMemberRole -> teamMemberRole.getRole().equals(industryTeamRole.name()))
        .map(TeamMemberRole::getTeam)
        .toList();
  }

  private List<WorkAreaItem> getIndustryWorkAreaItems(List<Team> teams, ArrayList<Condition> conditions) {
    var organisationGroupIds = teams.stream().map(Team::getEnergyPortalOrgGroupId).toList();

    if (organisationGroupIds.isEmpty()) {
      return Collections.emptyList();
    }
    var workAreaItemDtoList = workAreaItemDtoRepository.performQuery(
        conditions,
        SCAPS.ORGANISATION_GROUP_ID.in(organisationGroupIds));
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
            .comparing((WorkAreaItemDto dto) -> dto.status().getDisplayOrder()).reversed()
            .thenComparing(sortByItemDate(), Comparator.reverseOrder()))
        .map(workAreaItemDto -> new WorkAreaItem(
            new ScapId(workAreaItemDto.scapId()),
            workAreaItemDto.scapVersionNumber(),
            workAreaItemDto.reference(),
            organisationGroupsMap.getOrDefault(workAreaItemDto.organisationGroupId(), "MISSING OPERATOR"),
            workAreaItemDto.projectName(),
            workAreaItemDto.status(),
            inferStatus(workAreaItemDto),
            workAreaItemDto.resolutionDate() == null
                && UpdateRequestType.FURTHER_INFORMATION.equals(workAreaItemDto.updateRequestType())
                && workAreaItemDto.dueDate() != null,
            (workAreaItemDto.scapVersionNumber() > 1) && ScapDetailStatus.DRAFT.equals(workAreaItemDto.status()),
            workAreaItemDto.dueDate() != null
                ? workAreaItemDto.dueDate().format(DateTimeFormatter.ofPattern(DateUtils.SHORT_DATE))
                : null)
        )
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

