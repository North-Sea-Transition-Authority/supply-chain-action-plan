package uk.co.nstauthority.scap.application.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.energyportal.FieldService;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsServiceTest {

  @Mock
  ProjectDetailsRepository projectDetailsRepository;

  @Mock
  ProjectDetailTypeRepository projectDetailTypeRepository;

  @Mock
  Clock clock;

  @Mock
  FieldService fieldService;

  @InjectMocks
  ProjectDetailsService projectDetailsService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
  }

  @Test
  void getProjectTypesByProjectDetails() {
    var projectDetails = new ProjectDetails(scapDetail, Instant.now());
    var type1 = new ProjectDetailType(projectDetails, Instant.now());
    type1.setProjectType(ProjectType.CARBON_STORAGE_PERMIT);
    var type2 = new ProjectDetailType(projectDetails, Instant.now());
    type2.setProjectType(ProjectType.DECOMMISSIONING_PROGRAMME);
    var projectDetailTypes = Set.of(type1, type2);

    when(projectDetailTypeRepository.findAllByProjectDetails(projectDetails)).thenReturn(projectDetailTypes);

    var projectTypes = projectDetailsService.getProjectTypesByProjectDetails(projectDetails);

    assertThat(projectTypes).containsExactlyInAnyOrder(
        type1.getProjectType(),
        type2.getProjectType()
    );
  }

  @Test
  void getProjectDetailsByScapDetail() {
    var projectDetails = new ProjectDetails(scapDetail, Instant.now());

    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectDetails));

    var returnedProjectDetails = projectDetailsService.getProjectDetailsByScapDetail(scapDetail);

    assertThat(returnedProjectDetails).contains(projectDetails);
  }

  @Test
  void saveProjectDetails_newProjectDetails() {
    var createdTimestamp = Instant.ofEpochSecond(1666778603);
    var form = getFilledProjectDetailsForm();
    var startDate = LocalDate.of(
        form.getExpectedStartDate().getYearInputValue().getAsInteger().get(),
        form.getExpectedStartDate().getMonthInputValue().getAsInteger().get(),
        form.getExpectedStartDate().getDayInputValue().getAsInteger().get()
    );
    var endDate = LocalDate.of(
        form.getExpectedEndDate().getYearInputValue().getAsInteger().get(),
        form.getExpectedEndDate().getMonthInputValue().getAsInteger().get(),
        form.getExpectedEndDate().getDayInputValue().getAsInteger().get()
    );
    var projectDetailsArgumentCaptor = ArgumentCaptor.forClass(ProjectDetails.class);
    var projectDetailTypesArgumentCaptor = ArgumentCaptor.forClass(ProjectDetailType.class);
    var field = new Field(7235, "Test field", null, null, null, null);

    when(clock.instant()).thenReturn(createdTimestamp);
    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.empty());
    when(fieldService.getFieldById(form.getFieldId().getAsInteger().get(), "Get field name to save SCAP project details"))
        .thenReturn(Optional.of(field));

    projectDetailsService.saveProjectDetails(scapDetail, form);

    verify(projectDetailsRepository, times(1)).save(projectDetailsArgumentCaptor.capture());
    verify(projectDetailTypeRepository, times(2)).save(projectDetailTypesArgumentCaptor.capture());

    var savedProjectDetails = projectDetailsArgumentCaptor.getValue();
    var savedProjectDetailTypes = projectDetailTypesArgumentCaptor.getAllValues();

    assertThat(savedProjectDetails).extracting(
        ProjectDetails::getProjectName,
        ProjectDetails::getProjectCostEstimate,
        ProjectDetails::getEstimatedValueLocalContent,
        ProjectDetails::getFieldId,
        ProjectDetails::getFieldName,
        ProjectDetails::getPlannedExecutionStartDate,
        ProjectDetails::getPlannedCompletionDate,
        ProjectDetails::getCreatedTimestamp
    ).containsExactly(
        form.getProjectName().getInputValue(),
        form.getProjectCostEstimate().getAsBigDecimal().get(),
        form.getEstimatedValueLocalContent().getAsBigDecimal().get(),
        form.getFieldId().getAsInteger().get(),
        field.getFieldName(),
        startDate,
        endDate,
        createdTimestamp
    );

    assertThat(savedProjectDetailTypes).extracting(
        ProjectDetailType::getProjectDetails,
        ProjectDetailType::getCreatedTimestamp,
        ProjectDetailType::getProjectType
    ).containsExactlyInAnyOrder(
        tuple(savedProjectDetails, createdTimestamp, ProjectType.CARBON_STORAGE_PERMIT),
        tuple(savedProjectDetails, createdTimestamp, ProjectType.FIELD_DEVELOPMENT_PLAN)
    );
  }

  @Test
  void saveProjectDetails_updateProjectDetails_assertOnlyCreatesAddedAndOnlyDeletesRemoved() {
    var createdTimestamp = Instant.ofEpochSecond(1666778603);
    var form = getFilledProjectDetailsForm();
    var startDate = LocalDate.of(
        form.getExpectedStartDate().getYearInputValue().getAsInteger().get(),
        form.getExpectedStartDate().getMonthInputValue().getAsInteger().get(),
        form.getExpectedStartDate().getDayInputValue().getAsInteger().get()
    );
    var endDate = LocalDate.of(
        form.getExpectedEndDate().getYearInputValue().getAsInteger().get(),
        form.getExpectedEndDate().getMonthInputValue().getAsInteger().get(),
        form.getExpectedEndDate().getDayInputValue().getAsInteger().get()
    );

    var existingProjectDetails = new ProjectDetails(scapDetail, createdTimestamp);
    var existingProjectDetailType1 = new ProjectDetailType(existingProjectDetails, createdTimestamp);
    existingProjectDetailType1.setProjectType(ProjectType.CARBON_STORAGE_PERMIT);
    var existingProjectDetailType2 = new ProjectDetailType(existingProjectDetails, createdTimestamp);
    existingProjectDetailType2.setProjectType(ProjectType.DECOMMISSIONING_PROGRAMME);
    var existingProjectDetailTypes = Set.of(
        existingProjectDetailType1,
        existingProjectDetailType2
    );
    var field = new Field(7235, "Test field", null, null, null, null);

    var projectDetailsArgumentCaptor = ArgumentCaptor.forClass(ProjectDetails.class);
    var projectDetailTypesArgumentCaptor = ArgumentCaptor.forClass(ProjectDetailType.class);

    when(clock.instant()).thenReturn(createdTimestamp);
    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(existingProjectDetails));
    when(projectDetailTypeRepository.findAllByProjectDetails(existingProjectDetails)).thenReturn(existingProjectDetailTypes);
    when(fieldService.getFieldById(form.getFieldId().getAsInteger().get(), "Get field name to save SCAP project details"))
        .thenReturn(Optional.of(field));

    projectDetailsService.saveProjectDetails(scapDetail, form);

    verify(projectDetailsRepository, times(1)).save(projectDetailsArgumentCaptor.capture());
    verify(projectDetailTypeRepository, times(1)).save(projectDetailTypesArgumentCaptor.capture());
    verify(projectDetailTypeRepository, times(1)).delete(existingProjectDetailType2);
    verify(projectDetailTypeRepository, never()).delete(existingProjectDetailType1);

    var savedProjectDetails = projectDetailsArgumentCaptor.getValue();
    var savedProjectDetailType = projectDetailTypesArgumentCaptor.getValue();

    assertThat(savedProjectDetails).extracting(
        ProjectDetails::getProjectName,
        ProjectDetails::getProjectCostEstimate,
        ProjectDetails::getEstimatedValueLocalContent,
        ProjectDetails::getFieldId,
        ProjectDetails::getFieldName,
        ProjectDetails::getPlannedExecutionStartDate,
        ProjectDetails::getPlannedCompletionDate,
        ProjectDetails::getCreatedTimestamp
    ).containsExactly(
        form.getProjectName().getInputValue(),
        form.getProjectCostEstimate().getAsBigDecimal().get(),
        form.getEstimatedValueLocalContent().getAsBigDecimal().get(),
        form.getFieldId().getAsInteger().get(),
        field.getFieldName(),
        startDate,
        endDate,
        createdTimestamp
    );

    assertThat(savedProjectDetailType).extracting(
        ProjectDetailType::getProjectDetails,
        ProjectDetailType::getCreatedTimestamp,
        ProjectDetailType::getProjectType
    ).containsExactly(
        existingProjectDetails, createdTimestamp, ProjectType.FIELD_DEVELOPMENT_PLAN
    );
  }

  private ProjectDetailsForm getFilledProjectDetailsForm() {
    var form = new ProjectDetailsForm();
    form.setProjectName("Test project name");
    form.setProjectTypes(Set.of(
        ProjectType.CARBON_STORAGE_PERMIT, ProjectType.FIELD_DEVELOPMENT_PLAN
    ));
    form.setProjectCostEstimate("2.2");
    form.setEstimatedValueLocalContent("1.1");
    form.setFieldId(String.valueOf(7235));
    form.setStartDay("22");
    form.setStartMonth("1");
    form.setStartYear("2022");
    form.setEndDay("27");
    form.setEndMonth("12");
    form.setEndYear("2023");
    return form;
  }
}
