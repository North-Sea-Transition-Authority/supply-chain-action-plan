package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;

@ExtendWith(MockitoExtension.class)
class WorkAreaServiceTest {

  @Mock
  WorkAreaItemDtoRepository workAreaItemDtoRepository;

  @Mock
  OrganisationGroupService organisationGroupService;

  @Mock
  UserDetailService userDetailService;

  @Mock
  TeamService teamService;

  @InjectMocks
  WorkAreaService workAreaService;

  ServiceUserDetail userDetail;
  OrganisationGroup organisationGroup;

  @BeforeEach
  void setup() {
    userDetail = new ServiceUserDetail(1L, 1L, "John" , "Smith", "john.smith@example.com");
    organisationGroup = new OrganisationGroup(55, "CENTRICA", null, null, null, null);
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

    when(userDetailService.getUserDetail()).thenReturn(userDetail);
    when(teamService.getTeamsThatUserBelongsTo(userDetail)).thenReturn(List.of(team));
    when(workAreaItemDtoRepository.getAll()).thenReturn(List.of(workAreaItemDto));
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(workAreaItemDto.organisationGroupId()), WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));

    var views = workAreaService.getWorkAreaItems();

    assertThat(views).extracting(
        WorkAreaItem::scapId,
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

    when(userDetailService.getUserDetail()).thenReturn(userDetail);
    when(teamService.getTeamsThatUserBelongsTo(userDetail)).thenReturn(List.of(team));
    when(workAreaItemDtoRepository.getAllByOrganisationGroups(List.of(organisationGroup.getOrganisationGroupId())))
        .thenReturn(List.of(workAreaItemDto));
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(workAreaItemDto.organisationGroupId()), WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));

    var views = workAreaService.getWorkAreaItems();

    assertThat(views).extracting(
        WorkAreaItem::scapId,
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

    when(userDetailService.getUserDetail()).thenReturn(userDetail);
    when(teamService.getTeamsThatUserBelongsTo(userDetail)).thenReturn(List.of(team));
    when(workAreaItemDtoRepository.getAllByOrganisationGroups(List.of(orgGrpId)))
        .thenReturn(workAreaItemDtoList);
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(orgGrpId, orgGrpId, orgGrpId, orgGrpId),
        WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));

    var views = workAreaService.getWorkAreaItems();

    assertThat(views).extracting(
        WorkAreaItem::scapId
    ).containsExactly(
        newSubmittedScapDto.scapId(),
        oldSubmittedScapDto.scapId(),
        newDraftScapDto.scapId(),
        oldDraftScapDto.scapId()
    );
  }

  @Test
  void getWorkAreaItems_WhenNoTeam_VerifyNeverCallsRepository() {
    when(userDetailService.getUserDetail()).thenReturn(userDetail);
    when(teamService.getTeamsThatUserBelongsTo(userDetail)).thenReturn(Collections.emptyList());

    var views = workAreaService.getWorkAreaItems();

    assertThat(views).isEmpty();

    verify(workAreaItemDtoRepository, never()).getAll();
    verify(workAreaItemDtoRepository, never()).getAllByOrganisationGroups(any());
    verify(organisationGroupService, never()).getOrganisationGroupsByIds(any(), any());
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
