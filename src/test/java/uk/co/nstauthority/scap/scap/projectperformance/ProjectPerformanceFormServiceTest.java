package uk.co.nstauthority.scap.scap.projectperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import java.math.BigDecimal;
import java.time.LocalDate;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;

@ExtendWith(MockitoExtension.class)
class ProjectPerformanceFormServiceTest {

  @Mock
  ProjectPerformanceFormValidator projectPerformanceFormValidator;

  @InjectMocks
  ProjectPerformanceFormService projectPerformanceFormService;

  @Test
  void validate_VerifyCallsValidator() {
    var form = new ProjectPerformanceForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = projectPerformanceFormService.validate(form, bindingResult);

    verify(projectPerformanceFormValidator).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
  }

  @Test
  void getForm_ProjectNotCompleted() {
    var projectPerformance = new ProjectPerformance();
    projectPerformance.setProjectCompleted(false);
    projectPerformance.setStartDate(LocalDate.of(2000, 1, 1));
    projectPerformance.setCompletionDate(LocalDate.of(2002, 1, 1));
    projectPerformance.setOutturnCost(BigDecimal.valueOf(5.43));

    var form = projectPerformanceFormService.getForm(projectPerformance);

    assertThat(form).extracting(
        ProjectPerformanceForm::getIsProjectCompleted,
        form1 -> form1.getStartDay().getInputValue(),
        form1 -> form1.getStartMonth().getInputValue(),
        form1 -> form1.getStartYear().getInputValue(),
        form1 -> form1.getCompletionDay().getInputValue(),
        form1 -> form1.getCompletionMonth().getInputValue(),
        form1 -> form1.getCompletionYear().getInputValue(),
        form1 -> form1.getOutturnCost().getInputValue()
    ).containsExactly(
        YesNo.NO,
        null, null, null,
        null, null, null,
        null
    );
  }

  @Test
  void getForm_ProjectIsCompleted() {
    var projectPerformance = new ProjectPerformance();
    var startDate = LocalDate.of(2000, 1, 1);
    var endDate = LocalDate.of(2002, 1, 1);
    var outturnCost = BigDecimal.valueOf(5.43);
    projectPerformance.setProjectCompleted(true);
    projectPerformance.setStartDate(startDate);
    projectPerformance.setCompletionDate(endDate);
    projectPerformance.setOutturnCost(outturnCost);

    var form = projectPerformanceFormService.getForm(projectPerformance);

    assertThat(form).extracting(
        ProjectPerformanceForm::getIsProjectCompleted,
        form1 -> form1.getStartDate().getAsLocalDate().get(),
        form1 -> form1.getCompletionDate().getAsLocalDate().get(),
        form1 -> form1.getOutturnCost().getAsBigDecimal().get()
    ).containsExactly(
        YesNo.YES, startDate, endDate, outturnCost
    );
  }
}
