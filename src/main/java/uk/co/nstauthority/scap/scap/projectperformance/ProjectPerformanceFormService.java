package uk.co.nstauthority.scap.scap.projectperformance;

import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;

@Service
public class ProjectPerformanceFormService {

  private final ProjectPerformanceFormValidator projectPerformanceFormValidator;

  ProjectPerformanceFormService(ProjectPerformanceFormValidator projectPerformanceFormValidator) {
    this.projectPerformanceFormValidator = projectPerformanceFormValidator;
  }

  public BindingResult validate(ProjectPerformanceForm form, BindingResult bindingResult) {
    projectPerformanceFormValidator.validate(form, bindingResult);
    return bindingResult;
  }

  public ProjectPerformanceForm getForm(ProjectPerformance projectPerformance) {
    var form = new ProjectPerformanceForm();
    form.setProjectCompleted(projectPerformance.getProjectCompleted());
    if (Boolean.TRUE.equals(form.getProjectCompleted())) {
      var startDate = projectPerformance.getStartDate();
      form.setStartDay(String.valueOf(startDate.getDayOfMonth()));
      form.setStartMonth(String.valueOf(startDate.getMonthValue()));
      form.setStartYear(String.valueOf(startDate.getYear()));
      var endDate = projectPerformance.getCompletionDate();
      form.setCompletionDay(String.valueOf(endDate.getDayOfMonth()));
      form.setCompletionMonth(String.valueOf(endDate.getMonthValue()));
      form.setCompletionYear(String.valueOf(endDate.getYear()));
      form.setOutturnCost(projectPerformance.getOutturnCost().toPlainString());
    }
    return form;
  }
}
