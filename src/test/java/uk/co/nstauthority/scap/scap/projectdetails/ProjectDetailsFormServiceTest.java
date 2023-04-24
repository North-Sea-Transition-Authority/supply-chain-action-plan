package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.fivium.energyportalapi.generated.types.Facility;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.file.UploadedFileView;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsFormServiceTest {

  @Mock
  ProjectDetailsFormValidator projectDetailsFormValidator;

  @Mock
  ProjectDetailsService projectDetailsService;

  @Mock
  FieldService fieldService;

  @Mock
  FacilityService facilityService;

  @Mock
  SupportingDocumentService supportingDocumentService;

  @InjectMocks
  ProjectDetailsFormService projectDetailsFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var returnedBindingResult = projectDetailsFormService.validate(form, bindingResult);

    verify(projectDetailsFormValidator).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
  }

  @Test
  void getForm_assertFilledCorrectly() {
    var projectName = "Test project name";
    var projectTypes = Set.of(ProjectType.DECOMMISSIONING_PROGRAMME, ProjectType.FIELD_DEVELOPMENT_PLAN);
    var projectCostEstimate = BigDecimal.valueOf(12.3);
    var fieldIds = Collections.singleton(7235);
    var startDate = LocalDate.of(2000, 12, 30);
    var endDate = LocalDate.of(2003, 8, 11);

    var projectDetails = new ProjectDetails();
    projectDetails.setProjectName(projectName);
    projectDetails.setProjectCostEstimate(projectCostEstimate);
    projectDetails.setExpectsToMeetLocalContentCommitment(true);
    projectDetails.setPlannedExecutionStartDate(startDate);
    projectDetails.setPlannedCompletionDate(endDate);

    when(projectDetailsService.getProjectTypesByProjectDetails(projectDetails)).thenReturn(projectTypes);

    var form = projectDetailsFormService.getForm(projectDetails, null, fieldIds);

    assertThat(form).extracting(
        extractedForm -> extractedForm.getProjectName().getInputValue(),
        ProjectDetailsForm::getProjectTypes,
        extractedForm -> extractedForm.getProjectCostEstimate().getInputValue(),
        ProjectDetailsForm::getExpectsToMeetLocalContentCommitment,
        extractedForm -> extractedForm.getWillMissLocalContentCommitmentRationale().getInputValue(),
        ProjectDetailsForm::getFieldIds,
        ProjectDetailsForm::getHasPlatforms,
        extractedForm -> extractedForm.getStartDay().getInputValue(),
        extractedForm -> extractedForm.getStartMonth().getInputValue(),
        extractedForm -> extractedForm.getStartYear().getInputValue(),
        extractedForm -> extractedForm.getEndDay().getInputValue(),
        extractedForm -> extractedForm.getEndMonth().getInputValue(),
        extractedForm -> extractedForm.getEndYear().getInputValue()
    ).containsExactly(
        projectName,
        projectTypes,
        projectCostEstimate.toString(),
        true,
        null,
        fieldIds,
        null,
        String.valueOf(startDate.getDayOfMonth()),
        String.valueOf(startDate.getMonthValue()),
        String.valueOf(startDate.getYear()),
        String.valueOf(endDate.getDayOfMonth()),
        String.valueOf(endDate.getMonthValue()),
        String.valueOf(endDate.getYear())
    );

  }

  @Test
  void getForm_NoFacilities_AssertHasPlatformsIsNo() {
    var projectDetails = new ProjectDetails();
    projectDetails.setHasFacilities(false);
    projectDetails.setProjectCostEstimate(BigDecimal.valueOf(1));
    projectDetails.setExpectsToMeetLocalContentCommitment(true);
    projectDetails.setPlannedExecutionStartDate(LocalDate.of(1, 1, 1));
    projectDetails.setPlannedCompletionDate(LocalDate.of(1, 1, 1));

    var form = projectDetailsFormService.getForm(projectDetails, null, null);

    assertThat(form.getHasPlatforms()).isFalse();
  }

  @Test
  void getForm_HasFacilities_AssertHasPlatformsIsYes() {
    var projectDetails = new ProjectDetails();
    projectDetails.setHasFacilities(true);
    projectDetails.setProjectCostEstimate(BigDecimal.valueOf(1));
    projectDetails.setExpectsToMeetLocalContentCommitment(true);
    projectDetails.setPlannedExecutionStartDate(LocalDate.of(1, 1, 1));
    projectDetails.setPlannedCompletionDate(LocalDate.of(1, 1, 1));
    var projectFacilityIds = Set.of(1, 2, 3);

    var form = projectDetailsFormService.getForm(projectDetails, projectFacilityIds, null);

    assertThat(form.getHasPlatforms()).isTrue();
    assertThat(form.getInstallationIds()).isEqualTo(projectFacilityIds);
  }

  @Test
  void getPreselectedFields_existingField() {
    var fieldId = 22;
    var field = Field.newBuilder()
        .fieldId(fieldId)
        .fieldName("Test field")
        .build();
    var fields = Collections.singletonList(field);
    var requestPurpose = ProjectDetailsFormService.PRESELECTED_FIELD_REQUEST_PURPOSE;
    var projectFieldIds = Collections.singleton(fieldId);

    when(fieldService.getFieldsByIds(Collections.singletonList(fieldId), requestPurpose)).thenReturn(fields);

    var preselectedFields = projectDetailsFormService.getPreselectedFields(projectFieldIds);

    assertThat(preselectedFields).extracting(
        AddToListItem::getId, AddToListItem::getName, AddToListItem::isValid
    ).containsExactly(
        tuple(String.valueOf(fieldId), field.getFieldName(), true)
    );
  }

  @Test
  void getPreselectedFields_NoProjectFields() {
    var preselectedFields = projectDetailsFormService.getPreselectedFields(Collections.emptySet());

    assertThat(preselectedFields).isEmpty();

    verifyNoInteractions(fieldService);
  }

  @Test
  void getPreselectedFacilities() {
    var facilityId = 14;
    var projectFacilityIds = Collections.singleton(facilityId);
    var facility = new Facility(facilityId, "Test facility name", null, null, null);
    var facilities = List.of(facility);

    when(facilityService
        .findFacilitiesByIds(Collections.singletonList(facilityId), ProjectDetailsFormService.PRESELECTED_FACILITIES_REQUEST_PURPOSE))
        .thenReturn(facilities);

    var preselectedFacilities = projectDetailsFormService.getPreselectedFacilities(projectFacilityIds);

    assertThat(preselectedFacilities).extracting(
        AddToListItem::getId, AddToListItem::getName, AddToListItem::isValid
    ).containsExactly(
        tuple(String.valueOf(facility.getId()), facility.getName(), true)
    );
  }

  @Test
  void getPreselectedFields_NoProjectFacilities() {
    var preselectedFacilities = projectDetailsFormService.getPreselectedFacilities(Collections.emptySet());

    assertThat(preselectedFacilities).isEmpty();

    verifyNoInteractions(facilityService);
  }

  @Test
  void getSupportingDocuments() {
    var form = new ProjectDetailsForm();
    var fileUploadForm = new FileUploadForm();
    var uploadedFileId = UUID.randomUUID();
    fileUploadForm.setUploadedFileId(uploadedFileId);
    form.setSupportingDocuments(List.of(fileUploadForm));
    var uploadedFileView = new UploadedFileView(
        uploadedFileId.toString(), "file name", "1", "file desc", Instant.now()
    );

    when(supportingDocumentService.getUploadedFileViewList(List.of(uploadedFileId)))
        .thenReturn(List.of(uploadedFileView));

    var views = projectDetailsFormService.getSupportingDocuments(form);

    assertThat(views).containsExactly(uploadedFileView);
  }
}
