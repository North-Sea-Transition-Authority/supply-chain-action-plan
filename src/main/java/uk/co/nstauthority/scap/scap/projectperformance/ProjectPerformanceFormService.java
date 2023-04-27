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
      form.setStartDate(projectPerformance.getStartDate());
      form.setCompletionDate(projectPerformance.getCompletionDate());
      form.setOutturnCost(projectPerformance.getOutturnCost().toPlainString());
    }
    return form;
  }
}
