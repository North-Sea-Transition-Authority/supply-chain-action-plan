package uk.co.nstauthority.scap.scap.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getActualTenderSummaryView;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getPlannedTenderSummaryView;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getProjectDetailsSummaryView;

import java.math.BigDecimal;
import java.time.LocalDate;
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
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityBuilder;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;
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
  void addScapSummaryToModel_VerifyCalls() {
    var scapDetail = mock(ScapDetail.class);

    ScapSummaryControllerTestUtil.mockScapSummaryViewServiceMethods(scapSummaryViewService, scapDetail);

    var summaryView = scapSummaryViewService.getScapSummaryView(scapDetail);

    verify(scapSummaryViewService).getProjectDetailsSummaryView(scapDetail);
    verify(scapSummaryViewService).getPlannedTenderSummaryView(scapDetail);
    verify(scapSummaryViewService).getActualTenderSummaryView(scapDetail);

    assertThat(summaryView).extracting(
        ScapSummaryView::projectDetailsSummaryView,
        ScapSummaryView::plannedTenderSummaryView,
        ScapSummaryView::actualTenderSummaryView
    ).containsExactly(
        getProjectDetailsSummaryView(),
        getPlannedTenderSummaryView(),
        getActualTenderSummaryView()
    );
  }

  @ParameterizedTest
  @MethodSource("yesNoBooleanParameters")
  void getProjectDetailsSubmissionView_HasFacilities(Boolean hasFacilities, YesNo hasFacilitiesEnum) {
    var projectDetails = getValidProjectDetails();
    projectDetails.setHasFacilities(hasFacilities);
    var projectTypes = getValidProjectTypes();
    var facilities = List.of("Test facility");

    when(projectDetailsService.getProjectDetails(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectDetailsService.getProjectTypesByProjectDetails(projectDetails)).thenReturn(projectTypes);
    when(projectDetailsService.getProjectFacilityNames(projectDetails)).thenReturn(facilities);

    var projectDetailsView = scapSummaryViewService.getProjectDetailsSummaryView(scapDetail);

    assertThat(projectDetailsView).extracting(
        ProjectDetailsSummaryView::projectName,
        ProjectDetailsSummaryView::projectTypes,
        ProjectDetailsSummaryView::projectCostEstimate,
        ProjectDetailsSummaryView::estimatedValueLocalContent,
        ProjectDetailsSummaryView::fieldName,
        ProjectDetailsSummaryView::hasFacilities,
        ProjectDetailsSummaryView::projectFacilities,
        ProjectDetailsSummaryView::plannedExecutionStartDate,
        ProjectDetailsSummaryView::plannedCompletionDate
    ).containsExactly(
        projectDetails.getProjectName(),
        projectTypes.stream().toList(),
        projectDetails.getProjectCostEstimate(),
        projectDetails.getEstimatedValueLocalContent(),
        projectDetails.getFieldName(),
        hasFacilitiesEnum,
        facilities,
        "1 Jan 2023",
        "1 Jan 2024"
    );
  }

  @Test
  void getProjectDetailsSubmissionView_NoProjectDetails() {
    when(projectDetailsService.getProjectDetails(scapDetail)).thenReturn(Optional.empty());

    var projectDetailsView = scapSummaryViewService.getProjectDetailsSummaryView(scapDetail);

    assertThat(projectDetailsView).extracting(
        ProjectDetailsSummaryView::projectName,
        ProjectDetailsSummaryView::projectTypes,
        ProjectDetailsSummaryView::projectCostEstimate,
        ProjectDetailsSummaryView::estimatedValueLocalContent,
        ProjectDetailsSummaryView::fieldName,
        ProjectDetailsSummaryView::hasFacilities,
        ProjectDetailsSummaryView::projectFacilities,
        ProjectDetailsSummaryView::plannedExecutionStartDate,
        ProjectDetailsSummaryView::plannedCompletionDate
    ).containsExactly(
        null,
        Collections.emptyList(),
        null,
        null,
        null,
        null,
        Collections.emptyList(),
        null,
        null
    );
  }

  @Test
  void getPlannedTenderSummaryView_NoPlannedTenderEntity() {
    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());

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

    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.of(plannedTender));

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

    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.of(plannedTender));
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
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.empty());

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

    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));

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
            null, null, null, null
        )
    );

    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(List.of(actualTenderActivity));
    when(actualTenderSummaryViewService.getByActualTenderActivities(List.of(actualTenderActivity), scapDetail.getScap().getId()))
        .thenReturn(activityViews);

    var actualTenderSummaryView = scapSummaryViewService.getActualTenderSummaryView(scapDetail);

    assertThat(actualTenderSummaryView).extracting(
        ActualTenderSummaryView::hasActualTenderActivities,
        ActualTenderSummaryView::actualTenderActivitySummaryViews
    ).containsExactly(
        true, activityViews
    );
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
    var fieldName = "test field";
    var plannedExecutionStartDate = LocalDate.of(2023, 1, 1);
    var plannedCompletionDate = LocalDate.of(2024, 1, 1);

    var projectDetails = new ProjectDetails();
    projectDetails.setProjectName(projectName);
    projectDetails.setProjectCostEstimate(projectCostEstimate);
    projectDetails.setEstimatedValueLocalContent(estimatedValueLocalContent);
    projectDetails.setFieldName(fieldName);
    projectDetails.setHasFacilities(false);
    projectDetails.setPlannedExecutionStartDate(plannedExecutionStartDate);
    projectDetails.setPlannedCompletionDate(plannedCompletionDate);

    return projectDetails;
  }

  private Set<ProjectType> getValidProjectTypes() {
    return Set.of(ProjectType.DECOMMISSIONING_PROGRAMME, ProjectType.CARBON_STORAGE_PERMIT);
  }
}
