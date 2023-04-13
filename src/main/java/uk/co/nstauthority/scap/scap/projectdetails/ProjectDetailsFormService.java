package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.fivium.energyportalapi.generated.types.Facility;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.file.FileUploadUtils;
import uk.co.nstauthority.scap.file.UploadedFileView;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;

@Service
public class ProjectDetailsFormService {

  static final String PRESELECTED_FIELD_REQUEST_PURPOSE = "Get preselected field for SCAP project details form";
  static final String PRESELECTED_FACILITIES_REQUEST_PURPOSE = "Get preselected facilities for SCAP project details form";
  private final ProjectDetailsFormValidator validator;
  private final ProjectDetailsService projectDetailsService;
  private final FieldService fieldService;
  private final FacilityService facilityService;
  private final SupportingDocumentService supportingDocumentService;

  @Autowired
  ProjectDetailsFormService(ProjectDetailsFormValidator validator, ProjectDetailsService projectDetailsService,
                            FieldService fieldService, FacilityService facilityService,
                            SupportingDocumentService supportingDocumentService) {
    this.validator = validator;
    this.projectDetailsService = projectDetailsService;
    this.fieldService = fieldService;
    this.facilityService = facilityService;
    this.supportingDocumentService = supportingDocumentService;
  }

  public BindingResult validate(ProjectDetailsForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  public ProjectDetailsForm getForm(ProjectDetails projectDetails,
                             Set<Integer> projectFacilityIds,
                             Set<Integer> projectFieldIds) {
    var form = new ProjectDetailsForm();

    form.setProjectName(projectDetails.getProjectName());
    form.setProjectSummary(projectDetails.getProjectSummary());
    var projectTypes = projectDetailsService.getProjectTypesByProjectDetails(projectDetails);

    form.setProjectTypes(projectTypes);
    form.setProjectCostEstimate(projectDetails.getProjectCostEstimate().toString());
    form.setEstimatedValueLocalContent(projectDetails.getEstimatedValueLocalContent().toString());
    form.setFieldIds(projectFieldIds);

    form.setHasPlatforms(projectDetails.getHasFacilities());
    form.setInstallationIds(projectFacilityIds);

    var startDate = projectDetails.getPlannedExecutionStartDate();
    form.setStartDay(String.valueOf(startDate.getDayOfMonth()));
    form.setStartMonth(String.valueOf(startDate.getMonthValue()));
    form.setStartYear(String.valueOf(startDate.getYear()));

    var endDate = projectDetails.getPlannedCompletionDate();
    form.setEndDay(String.valueOf(endDate.getDayOfMonth()));
    form.setEndMonth(String.valueOf(endDate.getMonthValue()));
    form.setEndYear(String.valueOf(endDate.getYear()));

    var fileUploadForms = supportingDocumentService.getFileUploadFormListForScapDetailAndType(
        projectDetails.getScapDetail(), SupportingDocumentType.ADDITIONAL_DOCUMENT);
    form.setSupportingDocuments(fileUploadForms);

    return form;
  }

  List<AddToListItem> getPreselectedFacilities(Set<Integer> projectFacilityIds) {
    if (projectFacilityIds.isEmpty()) {
      return Collections.emptyList();
    }

    var facilities = facilityService
        .findFacilitiesByIds(List.copyOf(projectFacilityIds), PRESELECTED_FACILITIES_REQUEST_PURPOSE);
    return facilities.stream()
        .sorted(Comparator.comparing(Facility::getName))
        .map(facility -> new AddToListItem(facility.getId().toString(), facility.getName(), true))
        .toList();
  }

  List<AddToListItem> getPreselectedFields(Set<Integer> projectFieldIds) {
    if (projectFieldIds.isEmpty()) {
      return Collections.emptyList();
    }

    var fields = fieldService.getFieldsByIds(List.copyOf(projectFieldIds), PRESELECTED_FIELD_REQUEST_PURPOSE);
    return fields.stream()
        .sorted(Comparator.comparing(Field::getFieldName))
        .map(field -> new AddToListItem(field.getFieldId().toString(), field.getFieldName(), true))
        .toList();
  }

  public List<UploadedFileView> getSupportingDocuments(ProjectDetailsForm form) {
    var supportingDocumentUploadIdList = FileUploadUtils.getUploadIdList(form.getSupportingDocuments());
    return supportingDocumentService.getUploadedFileViewList(supportingDocumentUploadIdList);
  }
}
