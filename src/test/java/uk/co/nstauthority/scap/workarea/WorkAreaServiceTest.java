package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.jooq.Condition;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;

@ExtendWith(MockitoExtension.class)
class WorkAreaServiceTest {

  @Mock
  WorkAreaItemDtoRepository workAreaItemDtoRepository;

  @Mock
  OrganisationGroupService organisationGroupService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  CaseEventService caseEventService;

  @Mock
  TeamService teamService;

  @Mock
  WorkAreaFilterService workAreaFilterService;

  @InjectMocks
  WorkAreaService workAreaService;

  ServiceUserDetail userDetail;
  OrganisationGroup organisationGroup;
  WorkAreaFilter filter;

  @Captor
  ArgumentCaptor<List<Condition>> conditionsArgumentCaptor;

  @BeforeEach
  void setup() {
    userDetail = new ServiceUserDetail(1L, 1L, "John" , "Smith", "john.smith@example.com");
    organisationGroup = new OrganisationGroup(55, "CENTRICA", null, null, null, null);
    filter = new WorkAreaFilter();
  }

  @Test
  void getWorkAreaItems_WhenRegulator_GetAllScapSummaries() {
    var team = new Team();
    team.setTeamType(TeamType.REGULATOR);
    var workAreaItemDto = new WorkAreaItemDto(
        1,
        21,
        "TEST-REF",
        "Project name",
        organisationGroup.getOrganisationGroupId(),
        ScapDetailStatus.DRAFT,
        false,
        false,
        false,
        false,
        Instant.now(),
        Instant.now()
    );

    when(caseEventService.isFurtherInfoResponseOutstanding(new ScapId(1))).thenReturn(false);
    when(workAreaItemDtoRepository.performQuery(any())).thenReturn(List.of(workAreaItemDto));
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(workAreaItemDto.organisationGroupId()), WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));
    when(workAreaFilterService.getConditions(filter)).thenReturn(new ArrayList<>());

    var views = workAreaService.getWorkAreaItems(filter, true, Collections.singletonList(team));

    assertThat(views).extracting(workAreaItem ->
        workAreaItem.scapId().scapId(),
        WorkAreaItem::scapVersion,
        WorkAreaItem::reference,
        WorkAreaItem::operator,
        WorkAreaItem::projectName,
        WorkAreaItem::status,
        WorkAreaItem::submissionStage
    ).containsExactly(
        tuple(
            workAreaItemDto.scapId(),
            workAreaItemDto.scapVersionNumber(),
            workAreaItemDto.reference(),
            organisationGroup.getName(),
            workAreaItemDto.projectName(),
            ScapDetailStatus.DRAFT,
            ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING
        )
    );
    verify(workAreaItemDtoRepository, never()).getAllByOrganisationGroups(any());
    verify(workAreaFilterService).getConditions(filter);
    verify(workAreaItemDtoRepository).performQuery(conditionsArgumentCaptor.capture());

    assertThat(conditionsArgumentCaptor.getValue()).containsExactly(
        SCAP_DETAILS.STATUS.notEqual(ScapDetailStatus.DRAFT.getEnumName())
    );
  }

  @Test
  void getWorkAreaItems_WhenNotRegulator_VerifyGetAllByOrganisationGroups() {
    var team = new Team();
    team.setTeamType(TeamType.INDUSTRY);
    team.setEnergyPortalOrgGroupId(organisationGroup.getOrganisationGroupId());
    var workAreaItemDto = new WorkAreaItemDto(
       1,
        21,
        "TEST-REF",
        "Project name",
        organisationGroup.getOrganisationGroupId(),
        ScapDetailStatus.DRAFT,
        false,
        false,
        false,
        false,
        Instant.now(),
        Instant.now()
    );

    when(caseEventService.isFurtherInfoResponseOutstanding(new ScapId(1))).thenReturn(false);
    when(workAreaItemDtoRepository.performQuery(any()))
        .thenReturn(List.of(workAreaItemDto));
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(workAreaItemDto.organisationGroupId()), WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));

    var views = workAreaService
        .getWorkAreaItems(new WorkAreaFilter(), false, Collections.singletonList(team));

    assertThat(views).extracting(workAreaItem ->
        workAreaItem.scapId().scapId(),
        WorkAreaItem::scapVersion,
        WorkAreaItem::reference,
        WorkAreaItem::operator,
        WorkAreaItem::projectName,
        WorkAreaItem::status,
        WorkAreaItem::submissionStage
    ).containsExactly(
        tuple(
            workAreaItemDto.scapId(),
            workAreaItemDto.scapVersionNumber(),
            workAreaItemDto.reference(),
            organisationGroup.getName(),
            workAreaItemDto.projectName(),
            ScapDetailStatus.DRAFT,
            ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING
        )
    );

    verify(workAreaItemDtoRepository, never()).getAllByScapStatusNotIn(any());
    verify(scapDetailService).isUpdateInProgress(new ScapId(workAreaItemDto.scapId()));
  }

  @Test
  void getWorkAreaItems_WhenMultipleDraftAndSubmitted_AssertSortedBySubmittedLatestThenDraftCreatedLatest() {
    var orgGrpId = organisationGroup.getOrganisationGroupId();
    var team = new Team();
    team.setTeamType(TeamType.INDUSTRY);
    team.setEnergyPortalOrgGroupId(orgGrpId);
    var currentInstant = Instant.now();
    var oldInstant = currentInstant.minusSeconds(1);
    var oldDraftScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(1)
        .withScapDetailStatus(ScapDetailStatus.DRAFT)
        .withCreatedTimestamp(oldInstant)
        .build();
    var newDraftScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(2)
        .withScapDetailStatus(ScapDetailStatus.DRAFT)
        .withCreatedTimestamp(currentInstant)
        .build();
    var oldSubmittedScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(3)
        .withScapDetailStatus(ScapDetailStatus.SUBMITTED)
        .withSubmittedTimestamp(oldInstant)
        .build();
    var newSubmittedScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(4)
        .withScapDetailStatus(ScapDetailStatus.SUBMITTED)
        .withSubmittedTimestamp(currentInstant)
        .build();

    var workAreaItemDtoList = List.of(
        oldDraftScapDto, newDraftScapDto, oldSubmittedScapDto, newSubmittedScapDto
    );

    when(caseEventService.isFurtherInfoResponseOutstanding(new ScapId(4))).thenReturn(false);
    when(workAreaItemDtoRepository.performQuery(any())).thenReturn(workAreaItemDtoList);
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(orgGrpId, orgGrpId, orgGrpId, orgGrpId),
        WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));

    var views = workAreaService
        .getWorkAreaItems(new WorkAreaFilter(), false, Collections.singletonList(team));

    verify(scapDetailService, times(4)).isUpdateInProgress(any());

    assertThat(views).extracting(
        item -> item.scapId().scapId()
    ).containsExactly(
        newSubmittedScapDto.scapId(),
        oldSubmittedScapDto.scapId(),
        newDraftScapDto.scapId(),
        oldDraftScapDto.scapId()
    );
  }

  @Test
  void getWorkAreaItems_WhenNoTeam_VerifyNeverCallsRepository() {
    var views = workAreaService.getWorkAreaItems(new WorkAreaFilter(), false, Collections.emptyList());

    assertThat(views).isEmpty();

    verify(workAreaItemDtoRepository, never()).getAllByScapStatusNotIn();
    verify(workAreaItemDtoRepository, never()).getAllByOrganisationGroups(any());
    verify(organisationGroupService, never()).getOrganisationGroupsByIds(any(), any());
    verify(scapDetailService, never()).isUpdateInProgress(any());
  }

  @Test
  void inferStatus_WhenProjectClosedOut_ExpectIsProjectCompleted() {
    var workAreaItemDto = new WorkAreaItemDtoBuilder()
        .withProjectClosedOut(true)
        .build();

    var status = workAreaService.inferStatus(workAreaItemDto);

    assertThat(status).isEqualTo(ScapSubmissionStage.PROJECT_COMPLETED);
  }

  @Test
  void inferStatus_WhenHasContractingPerformance_ExpectIsContractingPerformance() {
    var workAreaItemDto = new WorkAreaItemDtoBuilder()
        .withHasContractingPerformance(true)
        .build();

    var status = workAreaService.inferStatus(workAreaItemDto);

    assertThat(status).isEqualTo(ScapSubmissionStage.CONTRACTING_PERFORMANCE);
  }

  @Test
  void inferStatus_WhenHasActualTender_ExpectIsActualTender() {
    var workAreaItemDto = new WorkAreaItemDtoBuilder()
        .withHasActualTender(true)
        .build();

    var status = workAreaService.inferStatus(workAreaItemDto);

    assertThat(status).isEqualTo(ScapSubmissionStage.ACTUAL_TENDER);
  }

  @Test
  void inferStatus_WhenHasPlannedTender_ExpectIsPlannedTender() {
    var workAreaItemDto = new WorkAreaItemDtoBuilder()
        .withHasPlannedTender(true)
        .build();

    var status = workAreaService.inferStatus(workAreaItemDto);

    assertThat(status).isEqualTo(ScapSubmissionStage.PLANNED_TENDER);
  }

  @Test
  void inferStatus_WhenProjectNameExists_ExpectIsContractingStrategyPending() {
    var workAreaItemDto = new WorkAreaItemDtoBuilder()
        .withProjectName("Test project name")
        .build();

    var status = workAreaService.inferStatus(workAreaItemDto);

    assertThat(status).isEqualTo(ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING);
  }

  @Test
  void inferStatus_WhenAllFalseAndNull_ExpectIsDraft() {
    var workAreaItemDto = new WorkAreaItemDtoBuilder().build();

    var status = workAreaService.inferStatus(workAreaItemDto);

    assertThat(status).isEqualTo(ScapSubmissionStage.DRAFT);
  }

  private class WorkAreaItemDtoBuilder {

    private Integer scapId = 1;
    private String projectName = null;
    private final Integer operatorId = organisationGroup.getOrganisationGroupId();
    private ScapDetailStatus status = ScapDetailStatus.DRAFT;
    private Boolean projectClosedOut = false;
    private Boolean hasContractingPerformance = false;
    private Boolean hasActualTender = false;
    private Boolean hasPlannedTender = false;
    private Instant createdTimestamp = Instant.now();
    private Instant submittedTimestamp = null;

    public WorkAreaItemDtoBuilder withScapId(Integer scapId) {
      this.scapId = scapId;
      return this;
    }

    public WorkAreaItemDtoBuilder withProjectName(String projectName) {
      this.projectName = projectName;
      return this;
    }

    public WorkAreaItemDtoBuilder withScapDetailStatus(ScapDetailStatus status) {
      this.status = status;
      return this;
    }

    public WorkAreaItemDtoBuilder withProjectClosedOut(Boolean projectClosedOut) {
      this.projectClosedOut = projectClosedOut;
      return this;
    }

    public WorkAreaItemDtoBuilder withHasContractingPerformance(Boolean hasContractingPerformance) {
      this.hasContractingPerformance = hasContractingPerformance;
      return this;
    }

    public WorkAreaItemDtoBuilder withHasActualTender(Boolean hasActualTender) {
      this.hasActualTender = hasActualTender;
      return this;
    }

    public WorkAreaItemDtoBuilder withHasPlannedTender(Boolean hasPlannedTender) {
      this.hasPlannedTender = hasPlannedTender;
      return this;
    }

    public WorkAreaItemDtoBuilder withCreatedTimestamp(Instant createdTimestamp) {
      this.createdTimestamp = createdTimestamp;
      return this;
    }

    public WorkAreaItemDtoBuilder withSubmittedTimestamp(Instant submittedTimestamp) {
      this.submittedTimestamp = submittedTimestamp;
      return this;
    }

    public WorkAreaItemDto build() {
      return new WorkAreaItemDto(
          scapId,
          2,
          "TEST-REF",
          projectName,
          operatorId,
          status,
          projectClosedOut,
          hasContractingPerformance,
          hasActualTender,
          hasPlannedTender,
          createdTimestamp,
          submittedTimestamp
      );
    }
  }
}
