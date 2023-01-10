package uk.co.nstauthority.scap.scap.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;
import static uk.co.nstauthority.scap.scap.projectdetails.ProjectType.FIELD_DEVELOPMENT_PLAN;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView;
import uk.co.nstauthority.scap.scap.timeline.TimelineEventService;

@ContextConfiguration(classes = ScapSummaryController.class)
class ScapSummaryControllerTest extends AbstractControllerTest {

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  ProjectDetailsService projectDetailsService;

  @MockBean
  ScapSummaryViewService scapSummaryViewService;

  @MockBean
  OrganisationGroupService organisationGroupService;

  @MockBean
  TimelineEventService timelineEventService;

  @MockBean
  EnergyPortalUserService energyPortalUserService;

  private static ScapId SCAP_ID = new ScapId(1000);

  @Test
  void renderSummary_fullSCAPDetails() throws Exception {
    var scap = new Scap();
    scap.setReference("TEST PROJECT NAME");

    var detail = new ScapDetail();
    detail.setStatus(ScapDetailStatus.DRAFT);
    detail.setScap(scap);

    var projectDetails = new ProjectDetails();
    projectDetails.setFieldName("TESTING FIELD");

    when(scapDetailService.getLatestScapDetailByScapId(SCAP_ID.scapId())).thenReturn(Optional.of(detail));
    when(projectDetailsService.getProjectDetails(detail)).thenReturn(Optional.of(projectDetails));
    when(scapSummaryViewService.getScapSummaryView(detail)).thenReturn(getSummaryView());
    when(scapSummaryViewService.inferSubmissionStatusFromSummary(any())).thenReturn(ScapSubmissionStage.DRAFT);

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(user(testUser)))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
  }

  @Test
  void renderSummary_NoScap_NotFoundException() throws Exception {
    var expectedErrorMessage = "Could not find details for SCAP of ID: %s".formatted(SCAP_ID);

    mockMvc.perform(get(
            ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID)))
            .with(user(testUser)))
        .andExpect(status().isNotFound())
        .andExpect(result -> assertThat(result.getResolvedException().getMessage()).isEqualTo(expectedErrorMessage));
  }

  private ScapSummaryView getSummaryView() {
    var projectDetailsSummaryView = new ProjectDetailsSummaryView("Project Name",
        List.of(FIELD_DEVELOPMENT_PLAN),
        new BigDecimal("5000.50"),
        new BigDecimal("5000.50"),
        "44/L01",
        YesNo.YES,
        Collections.emptyList(),
        "11-07-2024",
        "11-09-2024");

    var plannedTenderSummaryView = new PlannedTenderSummaryView(false,
        Collections.emptyList());

    var actualTenderSummaryView = new ActualTenderSummaryView(false,
        Collections.emptyList());

    return getScapSummaryView();
  }
}
