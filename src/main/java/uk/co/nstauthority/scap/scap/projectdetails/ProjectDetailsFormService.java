package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.enumutil.YesNo;
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

  BindingResult validate(ProjectDetailsForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  ProjectDetailsForm getForm(ProjectDetails projectDetails, List<Integer> projectFacilityIds) {
    var form = new ProjectDetailsForm();

    form.setProjectName(projectDetails.getProjectName());
    var projectTypes = projectDetailsService.getProjectTypesByProjectDetails(projectDetails);

    form.setProjectTypes(projectTypes);
    form.setProjectCostEstimate(projectDetails.getProjectCostEstimate().toString());
    form.setEstimatedValueLocalContent(projectDetails.getEstimatedValueLocalContent().toString());
    form.setFieldId(projectDetails.getFieldId().toString());

    if (Boolean.TRUE.equals(projectDetails.getHasFacilities())) {
      form.setHasPlatforms(YesNo.YES);
    } else if (Boolean.FALSE.equals(projectDetails.getHasFacilities())) {
      form.setHasPlatforms(YesNo.NO);
    }

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

  Optional<Map<String, String>> getPreselectedField(Integer fieldId) {
    var fieldOptional = fieldService.getFieldById(fieldId, PRESELECTED_FIELD_REQUEST_PURPOSE);
    return fieldOptional.map(field -> Map.of(String.valueOf(fieldId), field.getFieldName()));
  }

  List<AddToListItem> getPreselectedFacilities(List<Integer> projectFacilityIds) {
    var facilities = facilityService.findFacilitiesByIds(projectFacilityIds, PRESELECTED_FACILITIES_REQUEST_PURPOSE);
    return facilities.stream()
        .map(facility -> new AddToListItem(facility.getId().toString(), facility.getName(), true))
        .toList();
  }

  public List<UploadedFileView> getSupportingDocuments(ProjectDetailsForm form) {
    var supportingDocumentUploadIdList = FileUploadUtils.getUploadIdList(form.getSupportingDocuments());
    return supportingDocumentService.getUploadedFileViewList(supportingDocumentUploadIdList);
  }
}
