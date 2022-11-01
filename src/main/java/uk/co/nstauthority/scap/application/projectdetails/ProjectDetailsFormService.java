package uk.co.nstauthority.scap.application.projectdetails;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
class ProjectDetailsFormService {

  private final ProjectDetailsFormValidator validator;
  private final ProjectDetailsService projectDetailsService;

  @Autowired
  ProjectDetailsFormService(ProjectDetailsFormValidator validator, ProjectDetailsService projectDetailsService) {
    this.validator = validator;
    this.projectDetailsService = projectDetailsService;
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
}
