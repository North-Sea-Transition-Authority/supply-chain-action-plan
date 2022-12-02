package uk.co.nstauthority.scap.scap.projectperformance;

import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;

@Service
class ProjectPerformanceFormService {

  private final ProjectPerformanceFormValidator projectPerformanceFormValidator;

  ProjectPerformanceFormService(ProjectPerformanceFormValidator projectPerformanceFormValidator) {
    this.projectPerformanceFormValidator = projectPerformanceFormValidator;
  }

  BindingResult validate(ProjectPerformanceForm form, BindingResult bindingResult) {
    projectPerformanceFormValidator.validate(form, bindingResult);
    return bindingResult;
  }

  ProjectPerformanceForm getForm(ProjectPerformance projectPerformance) {
    var form = new ProjectPerformanceForm();
    if (Boolean.TRUE.equals(projectPerformance.getProjectCompleted())) {
      form.setIsProjectCompleted(YesNo.YES);
      var startDate = projectPerformance.getStartDate();
      form.setStartDay(String.valueOf(startDate.getDayOfMonth()));
      form.setStartMonth(String.valueOf(startDate.getMonthValue()));
      form.setStartYear(String.valueOf(startDate.getYear()));
      var endDate = projectPerformance.getCompletionDate();
      form.setCompletionDay(String.valueOf(endDate.getDayOfMonth()));
      form.setCompletionMonth(String.valueOf(endDate.getMonthValue()));
      form.setCompletionYear(String.valueOf(endDate.getYear()));
      form.setOutturnCost(projectPerformance.getOutturnCost().toPlainString());
    } else {
      form.setIsProjectCompleted(YesNo.NO);
    }
    return form;
  }
}
