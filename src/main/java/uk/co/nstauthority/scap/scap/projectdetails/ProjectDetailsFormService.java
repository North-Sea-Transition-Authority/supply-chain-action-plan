package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.Map;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.energyportal.FieldService;

@Service
class ProjectDetailsFormService {

  private final ProjectDetailsFormValidator validator;
  private final ProjectDetailsService projectDetailsService;
  private final FieldService fieldService;

  @Autowired
  ProjectDetailsFormService(ProjectDetailsFormValidator validator, ProjectDetailsService projectDetailsService,
                            FieldService fieldService) {
    this.validator = validator;
    this.projectDetailsService = projectDetailsService;
    this.fieldService = fieldService;
  }

  BindingResult validate(ProjectDetailsForm form, BindingResult bindingResult) {
    validator.validate(form, bindingResult);
    return bindingResult;
  }

  ProjectDetailsForm getForm(ProjectDetails projectDetails) {
    var form = new ProjectDetailsForm();
    form.setProjectName(projectDetails.getProjectName());
    var projectTypes = projectDetailsService.getProjectTypesByProjectDetails(projectDetails);
    form.setProjectTypes(projectTypes);
    form.setProjectCostEstimate(projectDetails.getProjectCostEstimate().toString());
    form.setEstimatedValueLocalContent(projectDetails.getEstimatedValueLocalContent().toString());
    form.setFieldId(projectDetails.getFieldId().toString());
    var startDate = projectDetails.getPlannedExecutionStartDate();
    form.setStartDay(String.valueOf(startDate.getDayOfMonth()));
    form.setStartMonth(String.valueOf(startDate.getMonthValue()));
    form.setStartYear(String.valueOf(startDate.getYear()));
    var endDate = projectDetails.getPlannedCompletionDate();
    form.setEndDay(String.valueOf(endDate.getDayOfMonth()));
    form.setEndMonth(String.valueOf(endDate.getMonthValue()));
    form.setEndYear(String.valueOf(endDate.getYear()));
    return form;
  }

  Optional<Map<String, String>> getPreselectedField(Integer fieldId) {
    var fieldOptional = fieldService.getFieldById(fieldId, "Get preselected field for project details form");
    return fieldOptional.map(field -> Map.of(String.valueOf(fieldId), field.getFieldName()));
  }
}
