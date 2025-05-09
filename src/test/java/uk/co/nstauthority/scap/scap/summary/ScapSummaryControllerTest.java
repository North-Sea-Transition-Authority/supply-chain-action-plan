package uk.co.nstauthority.scap.scap.summary;

import static java.util.Collections.singleton;
import static java.util.Collections.singletonList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole.SCAP_CASE_OFFICER;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import java.time.Instant;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.ResultMatcher;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamRole;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamView;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEvent;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventDocumentService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventView;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequest;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestType;

@ContextConfiguration(classes = ScapSummaryController.class)
class ScapSummaryControllerTest extends AbstractControllerTest {

  @MockBean
  ProjectDetailsService projectDetailsService;

  @MockBean
  ScapSummaryViewService scapSummaryViewService;

  @MockBean
  OrganisationGroupService organisationGroupService;

  @MockBean
  CaseEventService caseEventService;

  @MockBean
  UpdateRequestService updateRequestService;

  @MockBean
  EnergyPortalUserService energyPortalUserService;

  @MockBean
  CaseEventDocumentService caseEventDocumentService;

  private static final ScapId SCAP_ID = new ScapId(1000);

  private static final Integer SCAP_VERSION = 5;

  private ScapDetail scapDetail;

  private Scap scap;

  @BeforeEach
  void setup() {
    scap = new Scap(SCAP_ID);
    scap.setReference("TEST PROJECT NAME");
    scap.setOrganisationGroupId(1000);
    scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScap(scap)
        .withStatus(ScapDetailStatus.DRAFT)
        .withVersionNumber(SCAP_VERSION)
        .build();

    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(scapDetail);
    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);
    when(scapSummaryViewService.getScapSummaryView(scapDetail)).thenReturn(getScapSummaryView());
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.APPROVAL_DOCUMENT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.FURTHER_INFORMATION)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(teamService.getByEnergyPortalOrgGroupId(anyInt())).thenReturn(new Team());
    when(teamMemberService.findTeamMember(any(Team.class), eq(testUser.getWebUserAccountId())))
        .thenReturn(Optional.of(
            new TeamMember(testUser.getWebUserAccountId(), new TeamView(null, null, null), Set.of(SCAP_CASE_OFFICER))));
  }

  @Test
  void renderSummary_fullSCAPDetails() throws Exception {
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"))
        .andExpect(model().attributeExists("backLinkUrl"))
        .andExpect(model().attributeExists("updateScapUrl"))
        .andExpect(model().attributeExists("projectReference"))
        .andExpect(model().attributeExists("operator"))
        .andExpect(model().attributeExists("scapSummaryView"))
        .andExpect(model().attributeExists("caseEvents"))
        .andExpect(model().attributeExists("applicableActions"))
        .andExpect(model().attributeExists("updateInProgress"));
  }

  @Test
  void renderSummary_IsFirstDraft_UserIsSumbitter() throws Exception {
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(teamMemberService.getAllPermissionsForUser(testUser))
        .thenReturn(singletonList(RolePermission.SUBMIT_SCAP));
    scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScap(scap)
        .withStatus(ScapDetailStatus.DRAFT)
        .withVersionNumber(1)
        .build();
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(scapDetail);
    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))));
  }

  @Test
  void renderSummary_FirstVersionNotDraft_AssertNoRedirection() throws Exception {
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);

    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"))
        .andExpect(model().attributeExists("backLinkUrl"))
        .andExpect(model().attributeExists("updateScapUrl"))
        .andExpect(model().attributeExists("projectReference"))
        .andExpect(model().attributeExists("operator"))
        .andExpect(model().attributeExists("scapSummaryView"))
        .andExpect(model().attributeExists("caseEvents"))
        .andExpect(model().attributeExists("applicableActions"))
        .andExpect(model().attributeExists("updateInProgress"));
  }


  @Test
  void renderSummary_NotFirstVersionIsDraft_AssertNoRedirection() throws Exception {
    scapDetail.setVersionNumber(2);

    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"))
        .andExpect(model().attributeExists("backLinkUrl"))
        .andExpect(model().attributeExists("updateScapUrl"))
        .andExpect(model().attributeExists("projectReference"))
        .andExpect(model().attributeExists("operator"))
        .andExpect(model().attributeExists("scapSummaryView"))
        .andExpect(model().attributeExists("caseEvents"))
        .andExpect(model().attributeExists("applicableActions"))
        .andExpect(model().attributeExists("updateInProgress"))
        .andExpect(model().attributeExists("availableVersions"))
        .andExpect(model().attributeExists("versionSubmitUrl"));
  }


  @Test
  void renderSummary_RegulatorUser_CaseEventEmpty() throws Exception {
    var regulatorTeam = new Team();
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(true);
    when(teamService.getRegulatorTeam()).thenReturn(regulatorTeam);
    when(teamMemberService.getTeamMember(regulatorTeam, testUser.getWebUserAccountId())).thenReturn(
        TeamMemberTestUtil.Builder()
            .withRole(SCAP_CASE_OFFICER)
            .build()
    );
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
    verify(caseEventService).getEventViewByScapId(SCAP_ID);
  }

  @Test
  void renderSummary_IndustryUser_CaseEventEmpty() throws Exception {
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(false);
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
    verify(caseEventService, never()).getEventViewByScapId(SCAP_ID);
  }

  @Test
  void renderSummary_IndustryUser_andHasSubmitPermission_UpdateRequest() throws Exception {
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(false);
    when(teamMemberService.getAllPermissionsForUser(testUser.wuaId())).thenReturn(List.of(RolePermission.SUBMIT_SCAP));
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());
    when(updateRequestService.findNextDueUpdate(SCAP_ID)).thenReturn(Optional.of(getUpdateRequest()));
    when(teamService.findAllPermissionsForUserInOrganisationGroup(testUser.wuaId(), getUpdateRequest().getScap().getOrganisationGroupId()))
        .thenReturn(List.of(RolePermission.SUBMIT_SCAP));

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"))
        .andExpect(model().attribute("updateText", "TEST"))
        .andExpect(model().attribute("isUpdateable", true));
    verify(caseEventService, never()).getEventViewByScapId(SCAP_ID);
  }

  @ParameterizedTest
  @EnumSource(value = RolePermission.class, mode = EnumSource.Mode.EXCLUDE, names = "SUBMIT_SCAP")
  void renderSummary_IndustryUser_andNoSubmitPermission_UpdateRequest(RolePermission rolePermission) throws Exception {
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(false);
    when(teamMemberService.getAllPermissionsForUser(testUser.wuaId())).thenReturn(List.of(RolePermission.SUBMIT_SCAP));
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());
    when(updateRequestService.findNextDueUpdate(SCAP_ID)).thenReturn(Optional.of(getUpdateRequest()));
    when(teamService.findAllPermissionsForUserInOrganisationGroup(testUser.wuaId(), getUpdateRequest().getScap().getOrganisationGroupId()))
        .thenReturn(List.of(rolePermission));

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"))
        .andExpect(model().attribute("isUpdateable", false));
    verify(caseEventService, never()).getEventViewByScapId(SCAP_ID);
  }

  @Test
  void renderSummary_IndustryUser_VersionSelected() throws Exception {
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(false);
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(scapDetail);
    when(scapDetailService.getByScapIdAndVersionNumber(SCAP_ID, 4)).thenReturn(scapDetail);

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID, 4)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
    verify(caseEventService, never()).getEventViewByScapId(SCAP_ID);
    verify(scapDetailService).getByScapIdAndVersionNumber(SCAP_ID, 4);
  }

  @Test
  void renderSummary_RegulatorUser_VersionApplicable() throws Exception {
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(false);
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());
    when(scapDetailService.getAllVersionsForUser(scapDetail.getScap()))
        .thenReturn(getScapDetails());

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
    verify(caseEventService, never()).getEventViewByScapId(SCAP_ID);
  }

  @ParameterizedTest
  @MethodSource("permissionAnnotationCheck")
  void summaryController_securityAnnotations(Set<TeamRole> role, ResultMatcher status) throws Exception {
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(teamMemberService.findTeamMember(any(Team.class), eq(testUser.getWebUserAccountId())))
        .thenReturn(Optional.of(
            new TeamMember(testUser.getWebUserAccountId(), new TeamView(null, null, null), role)));
    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status);
  }

  public static Stream<Arguments> permissionAnnotationCheck() {
    var permittedPermissions = List.of(RolePermission.SUBMIT_SCAP, RolePermission.VIEW_SCAP);

    return Stream.concat(Stream.of(RegulatorTeamRole.values()), Stream.of(IndustryTeamRole.values()))
        .map(role -> Arguments.of(singleton(role),
            Collections.disjoint(((TeamRole) role).getRolePermissions(), permittedPermissions)
                ? status().isForbidden()
                : status().isOk()));
  }

  @ParameterizedTest
  @MethodSource("summaryStatusCheck")
  void summaryControllerRenderSummary_statusAnnotations(ScapDetailStatus detailStatus, ResultMatcher status) throws Exception {
    scapDetail.setStatus(detailStatus);
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(scapDetail);
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(authenticatedScapUser()))
        .andExpect(status);
  }

  @ParameterizedTest
  @MethodSource("summaryStatusCheck")
  void summaryControllerRenderVersionSummary_statusAnnotations(ScapDetailStatus detailStatus, ResultMatcher status) throws Exception {
    scapDetail.setStatus(detailStatus);
    when(scapDetailService.getByScapIdAndVersionNumber(SCAP_ID, 4)).thenReturn(scapDetail);
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID, 4)))
            .with(authenticatedScapUser()))
        .andExpect(status);
  }

  public static Stream<Arguments> summaryStatusCheck() {
    var permittedStatuses = List.of(
        ScapDetailStatus.DRAFT,
        ScapDetailStatus.SUBMITTED,
        ScapDetailStatus.APPROVED,
        ScapDetailStatus.CLOSED_OUT,
        ScapDetailStatus.WITHDRAWN);

    return Arrays.stream(ScapDetailStatus.values())
        .map(status -> Arguments.of(status,
            permittedStatuses.contains(status)
                ? status().isOk()
                : status().isBadRequest()));
  }

  private List<CaseEventView> getTimelineView() {
    var timelineEvent = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        SCAP_ID.scapId(),
        1,
        "",
        "TEST TESTER",
        "",
        null,
        null,
        null,
        null);
    return List.of(timelineEvent);
  }

  private List<ScapDetail> getScapDetails() {
    var list = new ArrayList<ScapDetail>();
    var scap = new Scap();
    var scapDetail = new ScapDetail();
    scapDetail.setVersionNumber(3);
    scapDetail.setStatus(ScapDetailStatus.DRAFT);
    scapDetail.setScap(scap);
    scapDetail.setCreatedTimestamp(Instant.now());
    list.add(scapDetail);

    var scapDetail2 = new ScapDetail();
    scapDetail2.setVersionNumber(2);
    scapDetail2.setStatus(ScapDetailStatus.SUBMITTED);
    scapDetail2.setScap(scap);
    scapDetail2.setCreatedTimestamp(Instant.now());
    list.add(scapDetail2);

    var scapDetail3 = new ScapDetail();
    scapDetail3.setVersionNumber(1);
    scapDetail3.setStatus(ScapDetailStatus.WITHDRAWN);
    scapDetail.setScap(scap);
    scapDetail3.setCreatedTimestamp(Instant.now());
    list.add(scapDetail3);

    return list;
  }

  @Test
  void getScapVersionSummary() throws Exception {
    var requestedVersion = 3;
    var expectedRedirectUrl = ReverseRouter.route(on(ScapSummaryController.class)
        .getScapSummary(SCAP_ID, requestedVersion));
    var form = new VersionSelectForm();
    form.setRequestedVersion(requestedVersion);

    mockMvc.perform(post(
        ReverseRouter.route(on(ScapSummaryController.class).getScapVersionSummary(SCAP_ID, null)))
            .with(csrf())
            .with(authenticatedScapUser())
            .flashAttr("versionSelectForm", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirectUrl));
  }

  private UpdateRequest getUpdateRequest() {
    var caseEvent = new CaseEvent();
    caseEvent.setComments("TEST");

    return new UpdateRequest(
        scap,
        UpdateRequestType.UPDATE,
        LocalDate.now(),
        caseEvent);
  }
}
