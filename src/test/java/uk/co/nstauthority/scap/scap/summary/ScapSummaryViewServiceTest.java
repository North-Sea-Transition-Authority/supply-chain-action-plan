package uk.co.nstauthority.scap.scap.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getActualTenderSummaryView;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getContractingPerformanceOverviewSummaryView;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getPlannedTenderSummaryView;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getProjectDetailsSummaryView;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getProjectPerformanceSummaryView;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.Month;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityBuilder;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformance;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceOverviewSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryView;
import uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryViewService;
import uk.co.nstauthority.scap.scap.summary.files.FileUploadSummaryViewService;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.plannedtender.PlannedTenderSummaryView;

@ExtendWith(MockitoExtension.class)
class ScapSummaryViewServiceTest {

  @Mock
  ProjectDetailsService projectDetailsService;

  @Mock
  PlannedTenderService plannedTenderService;

  @Mock
  PlannedTenderActivityService plannedTenderActivityService;

  @Mock
  ActualTenderService actualTenderService;

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @Mock
  ActualTenderSummaryViewService actualTenderSummaryViewService;

  @Mock
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @Mock
  ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;

  @Mock
  ProjectPerformanceService projectPerformanceService;

  @Mock
  FileUploadSummaryViewService fileUploadSummaryViewService;

  @InjectMocks
  ScapSummaryViewService scapSummaryViewService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapSummaryViewService = spy(scapSummaryViewService);
    var scap = new Scap(5482);
    scapDetail = new ScapDetail();
    scapDetail.setScap(scap);
  }

  @Test
  void getScapSummaryView_VerifyCalls() {
    var scapDetail = mock(ScapDetail.class);

    doReturn(getProjectDetailsSummaryView()).when(scapSummaryViewService).getProjectDetailsSummaryView(scapDetail);
    doReturn(getPlannedTenderSummaryView()).when(scapSummaryViewService).getPlannedTenderSummaryView(scapDetail);
    doReturn(getActualTenderSummaryView()).when(scapSummaryViewService).getActualTenderSummaryView(scapDetail);
    doReturn(getContractingPerformanceOverviewSummaryView()).when(scapSummaryViewService)
        .getContractingPerformanceOverviewSummaryView(scapDetail);
    doReturn(getProjectPerformanceSummaryView()).when(scapSummaryViewService).getProjectPerformanceSummaryView(scapDetail);

    var summaryView = scapSummaryViewService.getScapSummaryView(scapDetail);

    verify(scapSummaryViewService).getProjectDetailsSummaryView(scapDetail);
    verify(scapSummaryViewService).getPlannedTenderSummaryView(scapDetail);
    verify(scapSummaryViewService).getActualTenderSummaryView(scapDetail);
    verify(scapSummaryViewService).getContractingPerformanceOverviewSummaryView(scapDetail);
    verify(scapSummaryViewService).getProjectPerformanceSummaryView(scapDetail);

    assertThat(summaryView).extracting(
        ScapSummaryView::projectDetailsSummaryView,
        ScapSummaryView::plannedTenderSummaryView,
        ScapSummaryView::actualTenderSummaryView,
        ScapSummaryView::contractingPerformanceOverviewSummaryView,
        ScapSummaryView::projectPerformanceSummaryView
    ).containsExactly(
        getProjectDetailsSummaryView(),
        getPlannedTenderSummaryView(),
        getActualTenderSummaryView(),
        getContractingPerformanceOverviewSummaryView(),
        getProjectPerformanceSummaryView()
    );
  }

  @ParameterizedTest
  @MethodSource("yesNoBooleanParameters")
  void getProjectDetailsSubmissionView_HasFacilities(Boolean hasFacilities, YesNo hasFacilitiesEnum) {
    var projectDetails = getValidProjectDetails();
    projectDetails.setHasFacilities(hasFacilities);
    var projectTypes = getValidProjectTypes();
    var facilities = List.of("Test facility");
    var fields = Collections.singletonList("Test field");

    when(projectDetailsService.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsService.getProjectTypesByProjectDetails(projectDetails)).thenReturn(projectTypes);
    when(projectDetailsService.getProjectFacilityNames(projectDetails)).thenReturn(facilities);
    when(projectDetailsService.getProjectFieldNames(projectDetails)).thenReturn(fields);
    doReturn(Collections.emptyList()).when(fileUploadSummaryViewService)
        .getAllByScapDetailAndDocumentType(scapDetail, SupportingDocumentType.ADDITIONAL_DOCUMENT);

    var projectDetailsView = scapSummaryViewService.getProjectDetailsSummaryView(scapDetail);

    assertThat(projectDetailsView).extracting(
        ProjectDetailsSummaryView::projectName,
        ProjectDetailsSummaryView::projectTypes,
        ProjectDetailsSummaryView::projectCostEstimate,
        ProjectDetailsSummaryView::estimatedValueLocalContent,
        ProjectDetailsSummaryView::fieldNames,
        ProjectDetailsSummaryView::hasFacilities,
        ProjectDetailsSummaryView::projectFacilities,
        ProjectDetailsSummaryView::plannedExecutionStartDate,
        ProjectDetailsSummaryView::plannedCompletionDate,
        ProjectDetailsSummaryView::supportingDocuments
    ).containsExactly(
        projectDetails.getProjectName(),
        projectTypes.stream().toList(),
        projectDetails.getProjectCostEstimate(),
        projectDetails.getEstimatedValueLocalContent(),
        fields,
        hasFacilitiesEnum,
        facilities,
        "1 Jan 2023",
        "1 Jan 2024",
        Collections.emptyList()
    );
  }

  @Test
  void getProjectDetailsSubmissionView_NoProjectDetails() {
    when(projectDetailsService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    var projectDetailsView = scapSummaryViewService.getProjectDetailsSummaryView(scapDetail);

    assertThat(projectDetailsView).extracting(
        ProjectDetailsSummaryView::projectName,
        ProjectDetailsSummaryView::projectTypes,
        ProjectDetailsSummaryView::projectCostEstimate,
        ProjectDetailsSummaryView::estimatedValueLocalContent,
        ProjectDetailsSummaryView::fieldNames,
        ProjectDetailsSummaryView::hasFacilities,
        ProjectDetailsSummaryView::projectFacilities,
        ProjectDetailsSummaryView::plannedExecutionStartDate,
        ProjectDetailsSummaryView::plannedCompletionDate,
        ProjectDetailsSummaryView::supportingDocuments
    ).containsExactly(
        null,
        Collections.emptyList(),
        null,
        null,
        null,
        null,
        Collections.emptyList(),
        null,
        null,
        Collections.emptyList()
    );
  }

  @Test
  void getPlannedTenderSummaryView_NoPlannedTenderEntity() {
    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    var plannedTenderSummaryView = scapSummaryViewService.getPlannedTenderSummaryView(scapDetail);

    assertThat(plannedTenderSummaryView).extracting(
        PlannedTenderSummaryView::hasPlannedTender,
        PlannedTenderSummaryView::plannedTenderActivitySummaryViews
    ).containsExactly(
        null,
        null
    );
  }

  @Test
  void getPlannedTenderSummaryView_HasNoPlannedTenders() {
    var plannedTender = new PlannedTender();
    plannedTender.setHasPlannedTenders(false);

    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(plannedTender));

    var plannedTenderSummaryView = scapSummaryViewService.getPlannedTenderSummaryView(scapDetail);

    assertThat(plannedTenderSummaryView).extracting(
        PlannedTenderSummaryView::hasPlannedTender,
        PlannedTenderSummaryView::plannedTenderActivitySummaryViews
    ).containsExactly(
        plannedTender.getHasPlannedTenders(),
        Collections.emptyList()
    );

    verify(plannedTenderActivityService, never()).getTenderDetailsByPlannedTender(plannedTender);
  }

  @Test
  void getPlannedTenderSummaryView_HasPlannedTenders() {
    var plannedTender = new PlannedTender();
    plannedTender.setHasPlannedTenders(true);
    var plannedTenderActivity = getValidPlannedTenderActivity();

    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(plannedTender));
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(List.of(plannedTenderActivity));

    var plannedTenderSummaryView = scapSummaryViewService.getPlannedTenderSummaryView(scapDetail);

    assertThat(plannedTenderSummaryView.hasPlannedTender()).isTrue();
    assertThat(plannedTenderSummaryView.plannedTenderActivitySummaryViews()).extracting(
        PlannedTenderActivitySummaryView::awardRationale,
        PlannedTenderActivitySummaryView::estimatedValue,
        PlannedTenderActivitySummaryView::remunerationModel,
        PlannedTenderActivitySummaryView::remunerationModelName,
        PlannedTenderActivitySummaryView::scopeDescription
    ).containsExactly(
        tuple(
            plannedTenderActivity.getAwardRationale(),
            plannedTenderActivity.getEstimatedValue(),
            plannedTenderActivity.getRemunerationModel(),
            plannedTenderActivity.getRemunerationModelName(),
            plannedTenderActivity.getScopeDescription()
        )
    );
  }

  @Test
  void getActualTenderSummaryView_NoActualTenderEntity() {
    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    var actualTenderSummaryView = scapSummaryViewService.getActualTenderSummaryView(scapDetail);

    assertThat(actualTenderSummaryView).extracting(
        ActualTenderSummaryView::hasActualTenderActivities,
        ActualTenderSummaryView::actualTenderActivitySummaryViews
    ).containsExactly(
        null, null
    );
  }

  @Test
  void getActualTenderSummaryView_HasNoActualTenderActivities() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(false);

    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));

    var actualTenderSummaryView = scapSummaryViewService.getActualTenderSummaryView(scapDetail);

    assertThat(actualTenderSummaryView).extracting(
        ActualTenderSummaryView::hasActualTenderActivities,
        ActualTenderSummaryView::actualTenderActivitySummaryViews
    ).containsExactly(
        false, Collections.emptyList()
    );

    verify(actualTenderActivityService, never()).getAllByActualTender(any());
  }

  @Test
  void getActualTenderSummaryView_HasActualTenderActivities() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(true);
    var actualTenderActivity = new ActualTenderActivityBuilder()
        .withId(1)
        .build();
    var activityViews = List.of(
        new ActualTenderActivitySummaryView(
            null, null, null, null, null, null,
            null, null, null, null, true
        )
    );

    when(actualTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(List.of(actualTenderActivity));
    when(actualTenderSummaryViewService.getByActualTenderActivities(List.of(actualTenderActivity), scapDetail.getScap().getScapId()))
        .thenReturn(activityViews);

    var actualTenderSummaryView = scapSummaryViewService.getActualTenderSummaryView(scapDetail);

    assertThat(actualTenderSummaryView).extracting(
        ActualTenderSummaryView::hasActualTenderActivities,
        ActualTenderSummaryView::actualTenderActivitySummaryViews
    ).containsExactly(
        true, activityViews
    );
  }

  @Test
  void getContractingPerformanceOverviewSummaryView_NoContractingPerformanceOverview() {
    when(contractingPerformanceOverviewService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    var contractingPerformanceOverviewSummaryView = scapSummaryViewService.getContractingPerformanceOverviewSummaryView(scapDetail);

    assertThat(contractingPerformanceOverviewSummaryView).extracting(
        ContractingPerformanceOverviewSummaryView::hasContractingPerformance,
        ContractingPerformanceOverviewSummaryView::contractingPerformanceSummaryViews
    ).containsExactly(
        null,
        null
    );

    verify(contractingPerformanceSummaryViewService, never()).getContractingPerformanceSummaryViews(any());
  }

  @Test
  void getContractingPerformanceOverviewSummaryView_HasNoContractingPerformance() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(false);

    when(contractingPerformanceOverviewService.findByScapDetail(scapDetail)).thenReturn(Optional.of(contractingPerformanceOverview));

    var contractingPerformanceOverviewSummaryView = scapSummaryViewService.getContractingPerformanceOverviewSummaryView(scapDetail);

    assertThat(contractingPerformanceOverviewSummaryView).extracting(
        ContractingPerformanceOverviewSummaryView::hasContractingPerformance,
        ContractingPerformanceOverviewSummaryView::contractingPerformanceSummaryViews
    ).containsExactly(
        false,
        Collections.emptyList()
    );

    verify(contractingPerformanceSummaryViewService, never()).getContractingPerformanceSummaryViews(any());
  }

  @Test
  void getContractingPerformanceOverviewSummaryView_HasContractingPerformance() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(true);
    var contractingPerformanceViews = List.of(new ContractingPerformanceSummaryView(
        null, null, null, null, null, null,
        null, null, null, null, null
    ));

    when(contractingPerformanceOverviewService.findByScapDetail(scapDetail)).thenReturn(Optional.of(contractingPerformanceOverview));
    when(contractingPerformanceSummaryViewService.getContractingPerformanceSummaryViews(scapDetail.getScap().getScapId()))
        .thenReturn(contractingPerformanceViews);

    var contractingPerformanceOverviewSummaryView = scapSummaryViewService.getContractingPerformanceOverviewSummaryView(scapDetail);

    assertThat(contractingPerformanceOverviewSummaryView).extracting(
        ContractingPerformanceOverviewSummaryView::hasContractingPerformance,
        ContractingPerformanceOverviewSummaryView::contractingPerformanceSummaryViews
    ).containsExactly(
        true,
        contractingPerformanceViews
    );
  }

  @Test
  void getProjectPerformanceSummaryView_NoProjectPerformanceExists() {
    when(projectPerformanceService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    var projectPerformanceSummaryView = scapSummaryViewService.getProjectPerformanceSummaryView(scapDetail);

    assertThat(projectPerformanceSummaryView).extracting(
        ProjectPerformanceSummaryView::projectCompleted,
        ProjectPerformanceSummaryView::startDate,
        ProjectPerformanceSummaryView::completionDate,
        ProjectPerformanceSummaryView::outturnCost
    ).containsOnlyNulls();
  }

  @Test
  void getProjectPerformanceSummaryView_ProjectPerformanceExists() {
    var projectPerformance = new ProjectPerformance();
    projectPerformance.setProjectCompleted(true);
    var startDate = LocalDate.of(2000, Month.JANUARY, 1);
    projectPerformance.setStartDate(startDate);
    var completionDate = LocalDate.of(2010, Month.DECEMBER, 31);
    projectPerformance.setCompletionDate(completionDate);
    projectPerformance.setOutturnCost(BigDecimal.TEN);

    when(projectPerformanceService.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectPerformance));

    var projectPerformanceSummaryView = scapSummaryViewService.getProjectPerformanceSummaryView(scapDetail);

    assertThat(projectPerformanceSummaryView).extracting(
        ProjectPerformanceSummaryView::projectCompleted,
        ProjectPerformanceSummaryView::startDate,
        ProjectPerformanceSummaryView::completionDate,
        ProjectPerformanceSummaryView::outturnCost
    ).containsExactly(
        projectPerformance.getProjectCompleted(),
        DateUtils.format(startDate),
        DateUtils.format(completionDate),
        projectPerformance.getOutturnCost()
    );
  }

  @Test
  void inferStatus_WhenProjectCompleted_ExpectIsComplete() {
    var projectPerformanceSummaryView =
        new ProjectPerformanceSummaryView(true, "11-05-2023", "11-05-204", new BigDecimal("5000"));

    var contractingPerformanceSummaryView =
        new ContractingPerformanceOverviewSummaryView(true, Collections.emptyList());
    var actualTenderSummaryView =
        new ActualTenderSummaryView(true, Collections.emptyList());
    var plannedTenderSummaryView =
        new PlannedTenderSummaryView(true, Collections.emptyList());

    var scapSummary = new ScapSummaryView(getMockedProjectDetailsView(),
        plannedTenderSummaryView,
        actualTenderSummaryView,
        contractingPerformanceSummaryView,
        projectPerformanceSummaryView);

    var status = scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary);
    assertThat(status).isEqualTo(ScapSubmissionStage.PROJECT_COMPLETED);
  }

  @Test
  void inferStatus_WhenHasContractingPerformance_ExpectIsContractingPerformance() {
    var projectPerformanceSummaryView =
        new ProjectPerformanceSummaryView(false, "11-05-2023", "11-05-204", new BigDecimal("5000"));

    var contractingPerformanceSummaryView =
        new ContractingPerformanceOverviewSummaryView(true, Collections.emptyList());
    var actualTenderSummaryView =
        new ActualTenderSummaryView(true, Collections.emptyList());
    var plannedTenderSummaryView =
        new PlannedTenderSummaryView(true, Collections.emptyList());

    var scapSummary = new ScapSummaryView(getMockedProjectDetailsView(),
        plannedTenderSummaryView,
        actualTenderSummaryView,
        contractingPerformanceSummaryView,
        projectPerformanceSummaryView);

    var status = scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary);
    assertThat(status).isEqualTo(ScapSubmissionStage.CONTRACTING_PERFORMANCE);
  }

  @Test
  void inferStatus_WhenHasActualTender_ExpectIsActualTender() {
    var projectPerformanceSummaryView =
        new ProjectPerformanceSummaryView(false, "11-05-2023", "11-05-204", new BigDecimal("5000"));
    var contractingPerformanceSummaryView =
        new ContractingPerformanceOverviewSummaryView(false, Collections.emptyList());
    var actualTenderSummaryView =
        new ActualTenderSummaryView(true, Collections.emptyList());
    var plannedTenderSummaryView =
        new PlannedTenderSummaryView(true, Collections.emptyList());

    var scapSummary = new ScapSummaryView(getMockedProjectDetailsView(),
        plannedTenderSummaryView,
        actualTenderSummaryView,
        contractingPerformanceSummaryView,
        projectPerformanceSummaryView);

    var status = scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary);
    assertThat(status).isEqualTo(ScapSubmissionStage.ACTUAL_TENDER);
  }

  @Test
  void inferStatus_WhenHasPlannedTender_ExpectIsPlannedTender() {
    var projectPerformanceSummaryView =
        new ProjectPerformanceSummaryView(false, "11-05-2023", "11-05-204", new BigDecimal("5000"));
    var contractingPerformanceSummaryView =
        new ContractingPerformanceOverviewSummaryView(false, Collections.emptyList());
    var actualTenderSummaryView =
        new ActualTenderSummaryView(false, Collections.emptyList());
    var plannedTenderSummaryView =
        new PlannedTenderSummaryView(true, Collections.emptyList());

    var scapSummary = new ScapSummaryView(getMockedProjectDetailsView(),
        plannedTenderSummaryView,
        actualTenderSummaryView,
        contractingPerformanceSummaryView,
        projectPerformanceSummaryView);

    var status = scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary);
    assertThat(status).isEqualTo(ScapSubmissionStage.PLANNED_TENDER);
  }

  @Test
  void inferStatus_WhenHasProjectDetails_ExpectStratergyPending() {
    var projectPerformanceSummaryView =
        new ProjectPerformanceSummaryView(false, "11-05-2023", "11-05-204", new BigDecimal("5000"));
    var contractingPerformanceSummaryView =
        new ContractingPerformanceOverviewSummaryView(false, Collections.emptyList());
    var actualTenderSummaryView =
        new ActualTenderSummaryView(false, Collections.emptyList());
    var plannedTenderSummaryView =
        new PlannedTenderSummaryView(false, Collections.emptyList());

    var scapSummary = new ScapSummaryView(getMockedProjectDetailsView(),
        plannedTenderSummaryView,
        actualTenderSummaryView,
        contractingPerformanceSummaryView,
        projectPerformanceSummaryView);

    var status = scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary);
    assertThat(status).isEqualTo(ScapSubmissionStage.CONTRACTING_STRATEGY_PENDING);

  }

  private static Stream<Arguments> yesNoBooleanParameters() {
    return Stream.of(
        Arguments.of(true, YesNo.YES),
        Arguments.of(false, YesNo.NO),
        Arguments.of(null, null)
    );
  }

  private PlannedTenderActivity getValidPlannedTenderActivity() {
    var plannedTenderActivity = new PlannedTenderActivity();
    plannedTenderActivity.setAwardRationale("test award rationale");
    plannedTenderActivity.setEstimatedValue(BigDecimal.valueOf(1.2));
    plannedTenderActivity.setScopeDescription("test scope description");
    plannedTenderActivity.setRemunerationModel(RemunerationModel.OTHER);
    plannedTenderActivity.setRemunerationModelName("test remuneration model");
    return plannedTenderActivity;
  }

  private ProjectDetails getValidProjectDetails() {
    var projectName = "test project";
    var projectCostEstimate = BigDecimal.valueOf(12.345);
    var estimatedValueLocalContent = BigDecimal.valueOf(54.321);
    var plannedExecutionStartDate = LocalDate.of(2023, 1, 1);
    var plannedCompletionDate = LocalDate.of(2024, 1, 1);

    var projectDetails = new ProjectDetails();
    projectDetails.setProjectName(projectName);
    projectDetails.setProjectCostEstimate(projectCostEstimate);
    projectDetails.setEstimatedValueLocalContent(estimatedValueLocalContent);
    projectDetails.setHasFacilities(false);
    projectDetails.setPlannedExecutionStartDate(plannedExecutionStartDate);
    projectDetails.setPlannedCompletionDate(plannedCompletionDate);

    return projectDetails;
  }

  private Set<ProjectType> getValidProjectTypes() {
    return Set.of(ProjectType.DECOMMISSIONING_PROGRAMME, ProjectType.CARBON_STORAGE_PERMIT);
  }

  private ProjectDetailsSummaryView getMockedProjectDetailsView() {
    return new ProjectDetailsSummaryView("Test Project",
        "This is a test summary",
        List.of(ProjectType.DECOMMISSIONING_PROGRAMME),
        new BigDecimal("50000"),
        new BigDecimal("50000"),
        Collections.singletonList("BRENT"),
        YesNo.NO,
        Collections.emptyList(),
        "11-05-2023",
        "11-06-2023",
        Collections.emptyList());
  }
}
