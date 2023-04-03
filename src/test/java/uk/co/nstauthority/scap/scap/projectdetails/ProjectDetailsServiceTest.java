package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.assertj.core.groups.Tuple;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.Facility;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsServiceTest {

  @Mock
  ProjectDetailsRepository projectDetailsRepository;

  @Mock
  ProjectDetailTypeRepository projectDetailTypeRepository;

  @Mock
  ProjectFieldRepository projectFieldRepository;

  @Mock
  ProjectFacilityRepository projectFacilityRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @Mock
  FieldService fieldService;

  @Mock
  FacilityService facilityService;

  @Mock
  FileUploadService fileUploadService;

  @InjectMocks
  ProjectDetailsService projectDetailsService;

  @Captor
  private ArgumentCaptor<Set<ProjectFacility>> savedProjectFacilitiesArgumentCaptor;

  @Captor
  private ArgumentCaptor<Set<ProjectFacility>> deletedProjectFacilitiesArgumentCaptor;

  @Captor
  private ArgumentCaptor<Set<ProjectField>> savedProjectFieldsArgumentCaptor;

  @Captor
  private ArgumentCaptor<Set<ProjectField>> deletedProjectFieldsArgumentCaptor;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail();
  }

  @Test
  void getProjectTypesByProjectDetails() {
    var projectDetails = new ProjectDetails(scapDetail, clock.instant());
    var type1 = new ProjectDetailType(projectDetails, clock.instant());
    type1.setProjectType(ProjectType.CARBON_STORAGE_PERMIT);
    var type2 = new ProjectDetailType(projectDetails, clock.instant());
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
    var projectDetails = new ProjectDetails(scapDetail, clock.instant());

    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectDetails));

    var returnedProjectDetails = projectDetailsService.findByScapDetail(scapDetail);

    assertThat(returnedProjectDetails).contains(projectDetails);
  }

  @Test
  void getProjectDetailsOrThrow() {
    var projectDetails = new ProjectDetails(scapDetail, clock.instant());

    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectDetails));

    var returnedProjectDetails = projectDetailsService.getByScapDetail(scapDetail);

    assertThat(returnedProjectDetails).isEqualTo(projectDetails);
  }

  @Test
  void getProjectDetailsOrThrow_WhenNotPresent_AssertThrows() {
    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> projectDetailsService.getByScapDetail(scapDetail))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getProjectFacilityNames() {
    var projectDetails = new ProjectDetails();
    var projectFacilityId = 99;
    var projectFacility = new ProjectFacility();
    projectFacility.setFacilityId(projectFacilityId);
    var facilities = List.of(
        new Facility(projectFacilityId, "facility name", null, null, null)
    );

    when(projectFacilityRepository.findAllByProjectDetails(projectDetails)).thenReturn(List.of(projectFacility));
    when(facilityService.findFacilitiesByIds(
        List.of(projectFacilityId), ProjectDetailsService.PROJECT_FACILITIES_REQUEST_PURPOSE))
        .thenReturn(facilities);

    var facilityNames = projectDetailsService.getProjectFacilityNames(projectDetails);

    assertThat(facilityNames).containsExactly(facilities.get(0).getName());
  }

  @Test
  void getProjectFacilityNames_NoProjectFacilities() {
    var projectDetails = new ProjectDetails();

    when(projectFacilityRepository.findAllByProjectDetails(projectDetails)).thenReturn(Collections.emptyList());

    var facilityNames = projectDetailsService.getProjectFacilityNames(projectDetails);

    assertThat(facilityNames).isEmpty();
  }

  @Test
  void getProjectFieldNames() {
    var projectDetails = new ProjectDetails();
    var projectFieldId = 7;
    var projectFields = List.of(new ProjectField(projectDetails, projectFieldId, Instant.now()));
    var field = Field.newBuilder()
        .fieldId(1)
        .fieldName("test field 1")
        .build();

    when(projectFieldRepository.findAllByProjectDetails(projectDetails)).thenReturn(projectFields);
    when(fieldService.getFieldsByIds(List.of(projectFieldId), ProjectDetailsService.PROJECT_FIELDS_REQUEST_PURPOSE))
        .thenReturn(List.of(field));

    var projectFieldNames = projectDetailsService.getProjectFieldNames(projectDetails);

    assertThat(projectFieldNames).containsExactly(field.getFieldName());
  }

  @Test
  void getProjectFieldNames_NoProjectFields_AssertEmpty() {
    var projectDetails = new ProjectDetails();

    when(projectFieldRepository.findAllByProjectDetails(projectDetails)).thenReturn(Collections.emptyList());

    var projectFieldNames = projectDetailsService.getProjectFieldNames(projectDetails);

    assertThat(projectFieldNames).isEmpty();
  }

  @SuppressWarnings("OptionalGetWithoutIsPresent")
  @Test
  void saveProjectDetails_newProjectDetails() {
    var createdTimestamp = Instant.ofEpochSecond(1666778603);
    var form = getFilledProjectDetailsForm();
    var startDate = LocalDate.of(
        form.getExpectedStartDate().getYearInput().getAsInteger().get(),
        form.getExpectedStartDate().getMonthInput().getAsInteger().get(),
        form.getExpectedStartDate().getDayInput().getAsInteger().get()
    );
    var endDate = LocalDate.of(
        form.getExpectedEndDate().getYearInput().getAsInteger().get(),
        form.getExpectedEndDate().getMonthInput().getAsInteger().get(),
        form.getExpectedEndDate().getDayInput().getAsInteger().get()
    );
    var projectDetailsArgumentCaptor = ArgumentCaptor.forClass(ProjectDetails.class);
    var projectDetailTypesArgumentCaptor = ArgumentCaptor.forClass(ProjectDetailType.class);

    when(clock.instant()).thenReturn(createdTimestamp);
    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    projectDetailsService.saveProjectDetails(scapDetail, form);

    verify(projectDetailsRepository).save(projectDetailsArgumentCaptor.capture());
    verify(projectDetailTypeRepository, times(2)).save(projectDetailTypesArgumentCaptor.capture());

    var savedProjectDetails = projectDetailsArgumentCaptor.getValue();
    var savedProjectDetailTypes = projectDetailTypesArgumentCaptor.getAllValues();

    assertThat(savedProjectDetails).extracting(
        ProjectDetails::getProjectName,
        ProjectDetails::getProjectCostEstimate,
        ProjectDetails::getEstimatedValueLocalContent,
        ProjectDetails::getHasFacilities,
        ProjectDetails::getPlannedExecutionStartDate,
        ProjectDetails::getPlannedCompletionDate,
        ProjectDetails::getCreatedTimestamp
    ).containsExactly(
        form.getProjectName().getInputValue(),
        form.getProjectCostEstimate().getAsBigDecimal().get(),
        form.getEstimatedValueLocalContent().getAsBigDecimal().get(),
        false,
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

  @SuppressWarnings("OptionalGetWithoutIsPresent")
  @Test
  void saveProjectDetails_updateProjectDetails_assertOnlyCreatesAddedAndOnlyDeletesRemoved() {
    var createdTimestamp = Instant.ofEpochSecond(1666778603);
    var form = getFilledProjectDetailsForm();
    var startDate = LocalDate.of(
        form.getExpectedStartDate().getYearInput().getAsInteger().get(),
        form.getExpectedStartDate().getMonthInput().getAsInteger().get(),
        form.getExpectedStartDate().getDayInput().getAsInteger().get()
    );
    var endDate = LocalDate.of(
        form.getExpectedEndDate().getYearInput().getAsInteger().get(),
        form.getExpectedEndDate().getMonthInput().getAsInteger().get(),
        form.getExpectedEndDate().getDayInput().getAsInteger().get()
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

    var projectDetailsArgumentCaptor = ArgumentCaptor.forClass(ProjectDetails.class);
    var projectDetailTypesArgumentCaptor = ArgumentCaptor.forClass(ProjectDetailType.class);

    when(clock.instant()).thenReturn(createdTimestamp);
    when(projectDetailsRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(existingProjectDetails));
    when(projectDetailTypeRepository.findAllByProjectDetails(existingProjectDetails)).thenReturn(existingProjectDetailTypes);

    projectDetailsService.saveProjectDetails(scapDetail, form);

    verify(projectDetailsRepository).save(projectDetailsArgumentCaptor.capture());
    verify(projectDetailTypeRepository).save(projectDetailTypesArgumentCaptor.capture());
    verify(projectDetailTypeRepository).delete(existingProjectDetailType2);
    verify(projectDetailTypeRepository, never()).delete(existingProjectDetailType1);

    var savedProjectDetails = projectDetailsArgumentCaptor.getValue();
    var savedProjectDetailType = projectDetailTypesArgumentCaptor.getValue();

    assertThat(savedProjectDetails).extracting(
        ProjectDetails::getProjectName,
        ProjectDetails::getProjectCostEstimate,
        ProjectDetails::getEstimatedValueLocalContent,
        ProjectDetails::getHasFacilities,
        ProjectDetails::getPlannedExecutionStartDate,
        ProjectDetails::getPlannedCompletionDate,
        ProjectDetails::getCreatedTimestamp
    ).containsExactly(
        form.getProjectName().getInputValue(),
        form.getProjectCostEstimate().getAsBigDecimal().get(),
        form.getEstimatedValueLocalContent().getAsBigDecimal().get(),
        false,
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

  @Test
  void saveProjectDetails_HasPlatforms_VerifyRepositoryCalls() {
    var keptExistingFacility = new ProjectFacility(UUID.randomUUID());
    keptExistingFacility.setFacilityId(11);
    var removedExistingFacility = new ProjectFacility(UUID.randomUUID());
    removedExistingFacility.setFacilityId(22);
    var existingProjectFacilities = List.of(keptExistingFacility, removedExistingFacility);

    var addedFacilityId = 33;
    var form = getFilledProjectDetailsForm();
    form.setHasPlatforms(YesNo.YES);
    form.setInstallationIds(Set.of(addedFacilityId, keptExistingFacility.getFacilityId()));
    var projectDetails = new ProjectDetails();

    when(projectDetailsService.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectFacilityRepository.findAllByProjectDetails(projectDetails)).thenReturn(existingProjectFacilities);

    projectDetailsService.saveProjectDetails(scapDetail, form);

    verify(projectFacilityRepository).saveAll(savedProjectFacilitiesArgumentCaptor.capture());
    verify(projectFacilityRepository).deleteAll(deletedProjectFacilitiesArgumentCaptor.capture());

    assertThat(savedProjectFacilitiesArgumentCaptor.getValue()).extracting(
        ProjectFacility::getProjectDetails,
        ProjectFacility::getFacilityId
    ).containsExactly(
        Tuple.tuple(projectDetails, addedFacilityId)
    );

    assertThat(deletedProjectFacilitiesArgumentCaptor.getValue()).containsExactly(removedExistingFacility);
  }

  @Test
  void saveProjectDetails_VerifySavesAndDeletesProjectFields() {
    var projectDetails = new ProjectDetails();

    var keptExistingField = new ProjectField(projectDetails, 11, Instant.now());
    var removedExistingField = new ProjectField(projectDetails, 22, Instant.now());
    var existingProjectFields = List.of(keptExistingField, removedExistingField);

    var addedFieldId = 33;
    var form = getFilledProjectDetailsForm();
    var createdInstant = Instant.now();
    form.setFieldIds(Set.of(addedFieldId, keptExistingField.getFieldId()));

    when(projectDetailsService.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectDetails));
    when(projectFieldRepository.findAllByProjectDetails(projectDetails)).thenReturn(existingProjectFields);
    when(clock.instant()).thenReturn(createdInstant);

    projectDetailsService.saveProjectDetails(scapDetail, form);

    verify(projectFieldRepository).saveAll(savedProjectFieldsArgumentCaptor.capture());
    verify(projectFieldRepository).deleteAll(deletedProjectFieldsArgumentCaptor.capture());

    assertThat(savedProjectFieldsArgumentCaptor.getValue()).extracting(
        ProjectField::getProjectDetails,
        ProjectField::getFieldId,
        ProjectField::getCreatedTimestamp
    ).containsExactly(
        Tuple.tuple(projectDetails, addedFieldId, createdInstant)
    );

    assertThat(deletedProjectFieldsArgumentCaptor.getValue()).containsExactly(removedExistingField);
  }

  private ProjectDetailsForm getFilledProjectDetailsForm() {
    var form = new ProjectDetailsForm();
    form.setProjectName("Test project name");
    form.setProjectTypes(Set.of(
        ProjectType.CARBON_STORAGE_PERMIT, ProjectType.FIELD_DEVELOPMENT_PLAN
    ));
    form.setProjectCostEstimate("2.2");
    form.setEstimatedValueLocalContent("1.1");
    form.setFieldIds(Collections.singleton(7235));
    form.setHasPlatforms(YesNo.NO);
    form.setStartDay("22");
    form.setStartMonth("1");
    form.setStartYear("2022");
    form.setEndDay("27");
    form.setEndMonth("12");
    form.setEndYear("2023");
    return form;
  }
}
