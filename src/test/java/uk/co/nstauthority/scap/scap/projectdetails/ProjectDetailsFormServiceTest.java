package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.file.UploadedFileView;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
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
    var estimatedValueLocalContent = BigDecimal.valueOf(11.3);
    var fieldId = 7235;
    var startDate = LocalDate.of(2000, 12, 30);
    var endDate = LocalDate.of(2003, 8, 11);

    var projectDetails = new ProjectDetails();
    projectDetails.setProjectName(projectName);
    projectDetails.setProjectCostEstimate(projectCostEstimate);
    projectDetails.setEstimatedValueLocalContent(estimatedValueLocalContent);
    projectDetails.setFieldId(fieldId);
    projectDetails.setPlannedExecutionStartDate(startDate);
    projectDetails.setPlannedCompletionDate(endDate);

    when(projectDetailsService.getProjectTypesByProjectDetails(projectDetails)).thenReturn(projectTypes);

    var form = projectDetailsFormService.getForm(projectDetails, null);

    assertThat(form).extracting(
        extractedForm -> extractedForm.getProjectName().getInputValue(),
        ProjectDetailsForm::getProjectTypes,
        extractedForm -> extractedForm.getProjectCostEstimate().getInputValue(),
        extractedForm -> extractedForm.getEstimatedValueLocalContent().getInputValue(),
        extractedForm -> extractedForm.getFieldId().getInputValue(),
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
        estimatedValueLocalContent.toString(),
        String.valueOf(fieldId),
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
    projectDetails.setEstimatedValueLocalContent(BigDecimal.valueOf(1));
    projectDetails.setFieldId(1);
    projectDetails.setPlannedExecutionStartDate(LocalDate.of(1, 1, 1));
    projectDetails.setPlannedCompletionDate(LocalDate.of(1, 1, 1));

    var form = projectDetailsFormService.getForm(projectDetails, null);

    assertThat(form.getHasPlatforms()).isEqualTo(YesNo.NO);
  }

  @Test
  void getForm_HasFacilities_AssertHasPlatformsIsYes() {
    var projectDetails = new ProjectDetails();
    projectDetails.setHasFacilities(true);
    projectDetails.setProjectCostEstimate(BigDecimal.valueOf(1));
    projectDetails.setEstimatedValueLocalContent(BigDecimal.valueOf(1));
    projectDetails.setFieldId(1);
    projectDetails.setPlannedExecutionStartDate(LocalDate.of(1, 1, 1));
    projectDetails.setPlannedCompletionDate(LocalDate.of(1, 1, 1));
    var projectFacilityIds = List.of(1, 2, 3);

    var form = projectDetailsFormService.getForm(projectDetails, projectFacilityIds);

    assertThat(form.getHasPlatforms()).isEqualTo(YesNo.YES);
    assertThat(form.getInstallationIds()).isEqualTo(projectFacilityIds);
  }

  @Test
  void getPreselectedField_existingField() {
    var field = new Field(22, "Test field", null, null, null, null);
    var requestPurpose = ProjectDetailsFormService.PRESELECTED_FIELD_REQUEST_PURPOSE;
    when(fieldService.getFieldById(field.getFieldId(), requestPurpose)).thenReturn(Optional.of(field));

    var preselectedField = projectDetailsFormService.getPreselectedField(field.getFieldId());

    assertThat(preselectedField).contains(Map.of(String.valueOf(field.getFieldId()), field.getFieldName()));
  }

  @Test
  void getPreselectedField_nonExistentField_assertEmpty() {
    var fieldId = 22;
    var requestPurpose = ProjectDetailsFormService.PRESELECTED_FIELD_REQUEST_PURPOSE;
    when(fieldService.getFieldById(fieldId, requestPurpose)).thenReturn(Optional.empty());

    var preselectedField = projectDetailsFormService.getPreselectedField(fieldId);

    assertThat(preselectedField).isEmpty();
  }

  @Test
  void getPreselectedFacilities() {
    var projectFacilityIds = List.of(14);
    var facility = new Facility(14, "Test facility name", null, null, null);
    var facilities = List.of(facility);

    when(facilityService.findFacilitiesByIds(projectFacilityIds, ProjectDetailsFormService.PRESELECTED_FACILITIES_REQUEST_PURPOSE))
        .thenReturn(facilities);

    var preselectedFacilities = projectDetailsFormService.getPreselectedFacilities(projectFacilityIds);

    assertThat(preselectedFacilities).extracting(
        AddToListItem::getId, AddToListItem::getName, AddToListItem::isValid
    ).containsExactly(
        tuple(String.valueOf(facility.getId()), facility.getName(), true)
    );
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
