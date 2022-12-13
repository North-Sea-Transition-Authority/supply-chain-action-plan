package uk.co.nstauthority.scap.scap.submit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;
import uk.co.nstauthority.scap.scap.submit.submissionviews.ProjectDetailsSubmissionView;

@ExtendWith(MockitoExtension.class)
class SubmissionViewServiceTest {

  @Mock
  ProjectDetailsService projectDetailsService;

  @InjectMocks
  SubmissionViewService submissionViewService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
  }

  @ParameterizedTest
  @MethodSource("yesNoBooleanParameters")
  void getProjectDetailsSubmissionView_HasFacilities(Boolean hasFacilities, YesNo hasFacilitiesEnum) {
    var projectDetails = getValidProjectDetails();
    projectDetails.setHasFacilities(hasFacilities);
    var projectTypes = getValidProjectTypes();
    var facilities = List.of("Test facility");

    when(projectDetailsService.getProjectDetailsOrThrow(scapDetail)).thenReturn(projectDetails);
    when(projectDetailsService.getProjectTypesByProjectDetails(projectDetails)).thenReturn(projectTypes);
    when(projectDetailsService.getProjectFacilityNames(projectDetails)).thenReturn(facilities);

    var projectDetailsView = submissionViewService.getProjectDetailsSubmissionView(scapDetail);

    assertThat(projectDetailsView).extracting(
        ProjectDetailsSubmissionView::projectName,
        ProjectDetailsSubmissionView::projectTypes,
        ProjectDetailsSubmissionView::projectCostEstimate,
        ProjectDetailsSubmissionView::estimatedValueLocalContent,
        ProjectDetailsSubmissionView::fieldName,
        ProjectDetailsSubmissionView::hasFacilities,
        ProjectDetailsSubmissionView::projectFacilities,
        ProjectDetailsSubmissionView::plannedExecutionStartDate,
        ProjectDetailsSubmissionView::plannedCompletionDate
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

  private static Stream<Arguments> yesNoBooleanParameters() {
    return Stream.of(
        Arguments.of(true, YesNo.YES),
        Arguments.of(false, YesNo.NO),
        Arguments.of(null, null)
    );
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
