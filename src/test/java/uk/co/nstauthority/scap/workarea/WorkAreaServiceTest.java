package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAPS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;

import java.time.Instant;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
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
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

@ExtendWith(MockitoExtension.class)
class WorkAreaServiceTest {

  @Mock
  WorkAreaItemDtoRepository workAreaItemDtoRepository;

  @Mock
  OrganisationGroupService organisationGroupService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  WorkAreaFilterService workAreaFilterService;

  @Mock
  TeamMemberService teamMemberService;

  @Mock
  UserDetailService userDetailService;

  @InjectMocks
  WorkAreaService workAreaService;

  ServiceUserDetail userDetail;
  OrganisationGroup organisationGroup;
  WorkAreaFilter filter;

  @Captor
  ArgumentCaptor<List<Condition>> conditionsArgumentCaptor;

  @Captor
  ArgumentCaptor<Condition> conditionArgumentCaptor;

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
        Instant.now(),
        LocalDate.now(),
        LocalDate.now(),
        UpdateRequestType.FURTHER_INFORMATION
    );

    when(workAreaItemDtoRepository.performQuery(any(), any())).thenReturn(List.of(workAreaItemDto));
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
    verify(workAreaItemDtoRepository).performQuery(conditionsArgumentCaptor.capture(), conditionArgumentCaptor.capture());

    assertThat(conditionArgumentCaptor.getValue()).isEqualTo(
        SCAP_DETAILS.STATUS.notEqual(ScapDetailStatus.DRAFT.getEnumName())
    );
  }

  @Test
  void getWorkAreaItems_WhenNotRegulator_VerifyGetAllByOrganisationGroups() {
    when(userDetailService.getUserDetail()).thenReturn(userDetail);

    var teamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole(IndustryTeamRole.SCAP_SUBMITTER.name())
        .build();

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
        Instant.now(),
        null,
        LocalDate.now(),
        UpdateRequestType.FURTHER_INFORMATION
    );

    when(teamMemberService.findAllRolesByUser(List.of(team), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    when(workAreaItemDtoRepository.performQuery(any(), any()))
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
        WorkAreaItem::submissionStage,
        WorkAreaItem::outstandingInformationRequest,
        WorkAreaItem::updateInProgress,
        WorkAreaItem::requestDueBy
    ).containsExactly(
        tuple(
            workAreaItemDto.scapId(),
            workAreaItemDto.scapVersionNumber(),
            workAreaItemDto.reference(),
            organisationGroup.getName(),
            workAreaItemDto.projectName(),
            ScapDetailStatus.DRAFT,
            ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING,
            true,
            true,
            workAreaItemDto.dueDate().format(DateTimeFormatter.ofPattern(DateUtils.SHORT_DATE))
        )
    );

    verify(workAreaItemDtoRepository, never()).getAllByScapStatusNotIn(any());
  }

  @Test
  void getWorkAreaItems_verifyWorkAreaItemConditions() {
    when(userDetailService.getUserDetail()).thenReturn(userDetail);

    var teamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole(IndustryTeamRole.SCAP_SUBMITTER.name())
        .build();

    var team = new Team();
    team.setTeamType(TeamType.INDUSTRY);
    team.setEnergyPortalOrgGroupId(organisationGroup.getOrganisationGroupId());
    var workAreaItemDto = new WorkAreaItemDto(
        1,
        21,
        "TEST-REF",
        "Project name",
        organisationGroup.getOrganisationGroupId(),
        ScapDetailStatus.SUBMITTED,
        false,
        false,
        false,
        false,
        Instant.now(),
        Instant.now(),
        LocalDate.now(),
        null,
        UpdateRequestType.FURTHER_INFORMATION
    );

    when(teamMemberService.findAllRolesByUser(List.of(team), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    when(workAreaItemDtoRepository.performQuery(any(), any()))
        .thenReturn(List.of(workAreaItemDto));
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(workAreaItemDto.organisationGroupId()), WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));

    var views = workAreaService
        .getWorkAreaItems(new WorkAreaFilter(), false, Collections.singletonList(team));

    assertThat(views).extracting(
        WorkAreaItem::outstandingInformationRequest,
        WorkAreaItem::updateInProgress,
        WorkAreaItem::requestDueBy
    ).containsExactly(
        tuple(
            false,
            false,
            null
        )
    );

    verify(workAreaItemDtoRepository, never()).getAllByScapStatusNotIn(any());
  }


  @Test
  void getWorkAreaItems_WhenMultipleDraftAndSubmitted_AssertSortedBySubmittedLatestThenDraftCreatedLatest() {
    when(userDetailService.getUserDetail()).thenReturn(userDetail);

    var teamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole(IndustryTeamRole.SCAP_SUBMITTER.name())
        .build();

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

    when(teamMemberService.findAllRolesByUser(List.of(team), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    when(workAreaItemDtoRepository.performQuery(any(), any())).thenReturn(workAreaItemDtoList);
    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(orgGrpId, orgGrpId, orgGrpId, orgGrpId),
        WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(organisationGroup));

    var views = workAreaService
        .getWorkAreaItems(new WorkAreaFilter(), false, Collections.singletonList(team));

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
    when(userDetailService.getUserDetail()).thenReturn(userDetail);

    var views = workAreaService.getWorkAreaItems(new WorkAreaFilter(), false, Collections.emptyList());

    assertThat(views).isEmpty();

    verify(workAreaItemDtoRepository, never()).getAllByScapStatusNotIn();
    verify(workAreaItemDtoRepository, never()).getAllByOrganisationGroups(any());
    verify(organisationGroupService, never()).getOrganisationGroupsByIds(any(), any());
    verify(scapDetailService, never()).isUpdateInProgress(any());
  }

  @Test
  void getWorkAreaItems_whenUserHasViewAndSubmitRole_AssertNoDraftItems() {
    when(userDetailService.getUserDetail()).thenReturn(userDetail);

    var viewingTeamOrgId = 10;
    var viewingTeam = new Team();
    viewingTeam.setTeamType(TeamType.INDUSTRY);
    viewingTeam.setEnergyPortalOrgGroupId(viewingTeamOrgId);

    var viewerTeamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole(IndustryTeamRole.SCAP_VIEWER.name())
        .withTeam(viewingTeam)
        .build();

    var submittingTeamOrgId = 20;
    var submittingTeam = new Team();
    submittingTeam.setTeamType(TeamType.INDUSTRY);
    submittingTeam.setEnergyPortalOrgGroupId(submittingTeamOrgId);

    var submitterTeamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole(IndustryTeamRole.SCAP_SUBMITTER.name())
        .withTeam(submittingTeam)
        .build();

    var draftOnViewerTeamScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(1)
        .withOperatorId(viewingTeamOrgId)
        .withScapDetailStatus(ScapDetailStatus.DRAFT)
        .build();
    var draftOnSubmitterTeamScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(2)
        .withOperatorId(submittingTeamOrgId)
        .withScapDetailStatus(ScapDetailStatus.DRAFT)
        .build();
    var submittedOnViewerTeamScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(3)
        .withOperatorId(viewingTeamOrgId)
        .withScapDetailStatus(ScapDetailStatus.SUBMITTED)
        .build();
    var submittedOnSubmitterTeamScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(4)
        .withOperatorId(submittingTeamOrgId)
        .withScapDetailStatus(ScapDetailStatus.SUBMITTED)
        .build();

    var viewerWorkAreaItemDtoList = List.of(draftOnViewerTeamScapDto, submittedOnViewerTeamScapDto);
    var submitterWorkAreaItemDtoList = List.of(draftOnSubmitterTeamScapDto, submittedOnSubmitterTeamScapDto);


    when(teamMemberService.findAllRolesByUser(List.of(viewingTeam, submittingTeam), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(viewerTeamMemberRole, submitterTeamMemberRole));

    when(workAreaItemDtoRepository.performQuery(any(), eq(SCAPS.ORGANISATION_GROUP_ID.in(viewingTeamOrgId))))
        .thenReturn(viewerWorkAreaItemDtoList);
    when(workAreaItemDtoRepository.performQuery(any(), eq(SCAPS.ORGANISATION_GROUP_ID.in(submittingTeamOrgId))))
        .thenReturn(submitterWorkAreaItemDtoList);

    var viewingTeamOrgGroup = OrganisationGroup.newBuilder().name("viewing team").organisationGroupId(viewingTeamOrgId).build();
    var submittingTeamOrgGroup = OrganisationGroup.newBuilder().name("submitting team").organisationGroupId(submittingTeamOrgId).build();

    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(submittingTeamOrgId, submittingTeamOrgId),
        WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(submittingTeamOrgGroup));

    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(viewingTeamOrgId, viewingTeamOrgId),
        WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(viewingTeamOrgGroup));

    var views = workAreaService
        .getWorkAreaItems(new WorkAreaFilter(), false, List.of(viewingTeam, submittingTeam));

    assertThat(views).extracting(
        item -> item.scapId().scapId()
    ).containsExactly(
        submittedOnSubmitterTeamScapDto.scapId(),
        submittedOnViewerTeamScapDto.scapId(),
        draftOnSubmitterTeamScapDto.scapId()
    );
  }

  @Test
  void getWorkAreaItems_whenUserHasViewAndSubmitRoleInTheSameTeam_AssertNoDuplicates() {
    when(userDetailService.getUserDetail()).thenReturn(userDetail);

    var teamOrgId = 10;
    var team = new Team();
    team.setTeamType(TeamType.INDUSTRY);
    team.setEnergyPortalOrgGroupId(teamOrgId);

    var viewerTeamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole(IndustryTeamRole.SCAP_VIEWER.name())
        .withTeam(team)
        .build();

    var submitterTeamMemberRole = TeamMemberRoleTestUtil.Builder()
        .withRole(IndustryTeamRole.SCAP_SUBMITTER.name())
        .withTeam(team)
        .build();

    var draftScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(1)
        .withOperatorId(teamOrgId)
        .withScapDetailStatus(ScapDetailStatus.DRAFT)
        .build();
    var submittedScapDto = new WorkAreaItemDtoBuilder()
        .withScapId(3)
        .withOperatorId(teamOrgId)
        .withScapDetailStatus(ScapDetailStatus.SUBMITTED)
        .build();

    var workAreaItemDtoList = List.of(draftScapDto, submittedScapDto);

    when(teamMemberService.findAllRolesByUser(List.of(team), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(viewerTeamMemberRole, submitterTeamMemberRole));

    when(workAreaItemDtoRepository.performQuery(any(), eq(SCAPS.ORGANISATION_GROUP_ID.in(teamOrgId))))
        .thenReturn(workAreaItemDtoList);

    var teamOrgGroup = OrganisationGroup.newBuilder().name("team").organisationGroupId(teamOrgId).build();

    when(organisationGroupService.getOrganisationGroupsByIds(
        List.of(teamOrgId, teamOrgId),
        WorkAreaService.ORGANISATION_GROUPS_REQUEST_PURPOSE))
        .thenReturn(List.of(teamOrgGroup));

    var views = workAreaService.getWorkAreaItems(new WorkAreaFilter(), false, List.of(team));

    assertThat(views)
        .extracting(item -> item.scapId().scapId())
        .containsExactly(submittedScapDto.scapId(), draftScapDto.scapId());
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
    private Integer operatorId = organisationGroup.getOrganisationGroupId();
    private ScapDetailStatus status = ScapDetailStatus.DRAFT;
    private Boolean projectClosedOut = false;
    private Boolean hasContractingPerformance = false;
    private Boolean hasActualTender = false;
    private Boolean hasPlannedTender = false;
    private Instant createdTimestamp = Instant.now();
    private Instant submittedTimestamp = null;
    private LocalDate resolutionDate = LocalDate.now();
    private LocalDate dueDate = LocalDate.now();
    private UpdateRequestType updateRequestType = UpdateRequestType.FURTHER_INFORMATION;

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

    public WorkAreaItemDtoBuilder withOperatorId(Integer operatorId) {
      this.operatorId = operatorId;
      return this;
    }

    public WorkAreaItemDtoBuilder withResolutionDate(LocalDate resolutionDate) {
      this.resolutionDate = resolutionDate;
      return this;
    }

    public WorkAreaItemDtoBuilder withDueDate(LocalDate dueDate) {
      this.dueDate = dueDate;
      return this;
    }

    public WorkAreaItemDtoBuilder withUpdateRequestType(UpdateRequestType updateRequestType) {
      this.updateRequestType = updateRequestType;
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
          submittedTimestamp,
          resolutionDate,
          dueDate,
          updateRequestType
      );
    }
  }
}
