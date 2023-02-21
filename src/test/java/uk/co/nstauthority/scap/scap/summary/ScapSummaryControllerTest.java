package uk.co.nstauthority.scap.scap.summary;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;
import static uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject.FURTHER_INFO_REQUESTED;
import static uk.co.nstauthority.scap.scap.projectdetails.ProjectType.FIELD_DEVELOPMENT_PLAN;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventView;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView;

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
  EnergyPortalUserService energyPortalUserService;

  @MockBean
  SupportingDocumentService supportingDocumentService;

  private ScapDetail scapDetail;

  private static final ScapId SCAP_ID = new ScapId(1000);

  @BeforeEach
  void setup() {
    var scap = new Scap(SCAP_ID);
    scap.setReference("TEST PROJECT NAME");
    scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
    scapDetail.setStatus(ScapDetailStatus.DRAFT);


    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(scapService.getScapById(SCAP_ID.scapId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapSummaryViewService.getScapSummaryView(scapDetail)).thenReturn(getScapSummaryView());
    when(supportingDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(supportingDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.APPROVAL_DOCUMENT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(supportingDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.FURTHER_INFORMATION)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
  }

  @Test
  void renderSummary_fullSCAPDetails() throws Exception {

    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getApplicableActionsForScap(SCAP_ID)).thenReturn(Set.of(FURTHER_INFO_REQUESTED));

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"))
        .andExpect(model().attribute("applicableActions", Set.of(FURTHER_INFO_REQUESTED)));
  }


  @Test
  void renderSummary_RegulatorUser_CaseEventEmpty() throws Exception {
    when(supportingDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(true);
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());

    mockMvc.perform(get(
        ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
    verify(caseEventService).getEventViewByScapId(SCAP_ID);
  }

  @Test
  void renderSummary_IndustryUser_CaseEventEmpty() throws Exception {
    when(supportingDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(false);
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);
    when(caseEventService.getEventViewByScapId(SCAP_ID)).thenReturn(getTimelineView());

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
    verify(caseEventService, never()).getEventViewByScapId(SCAP_ID);
  }

  private ScapSummaryView getSummaryView() {
    var projectDetailsSummaryView = new ProjectDetailsSummaryView("Project Name",
        List.of(FIELD_DEVELOPMENT_PLAN),
        new BigDecimal("5000.50"),
        new BigDecimal("5000.50"),
        Collections.singletonList("BRENT"),
        YesNo.YES,
        Collections.emptyList(),
        "11-07-2024",
        "11-09-2024",
        null);

    var plannedTenderSummaryView = new PlannedTenderSummaryView(false,
        Collections.emptyList());

    var actualTenderSummaryView = new ActualTenderSummaryView(false,
        Collections.emptyList());

    return getScapSummaryView();
  }

  private List<CaseEventView> getTimelineView() {
    var timelineEvent = new CaseEventView(CaseEventSubject.SCAP_SUBMITTED.getDisplayName(),
        SCAP_ID.scapId(),
        1,
        "",
        "TEST TESTER",
        "",
        null);
    return List.of(timelineEvent);
  }
}
