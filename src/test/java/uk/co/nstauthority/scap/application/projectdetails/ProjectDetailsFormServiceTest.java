package uk.co.nstauthority.scap.application.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsFormServiceTest {

  @Mock
  ProjectDetailsFormValidator projectDetailsFormValidator;

  @Mock
  ProjectDetailsService projectDetailsService;

  @InjectMocks
  ProjectDetailsFormService projectDetailsFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var returnedBindingResult = projectDetailsFormService.validate(form, bindingResult);

    verify(projectDetailsFormValidator, times(1)).validate(form, bindingResult);
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

    var form = projectDetailsFormService.getForm(projectDetails);

    assertThat(form).extracting(
        extractedForm -> extractedForm.getProjectName().getInputValue(),
        ProjectDetailsForm::getProjectTypes,
        extractedForm -> extractedForm.getProjectCostEstimate().getInputValue(),
        extractedForm -> extractedForm.getEstimatedValueLocalContent().getInputValue(),
        extractedForm -> extractedForm.getFieldId().getInputValue(),
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
        String.valueOf(startDate.getDayOfMonth()),
        String.valueOf(startDate.getMonthValue()),
        String.valueOf(startDate.getYear()),
        String.valueOf(endDate.getDayOfMonth()),
        String.valueOf(endDate.getMonthValue()),
        String.valueOf(endDate.getYear())
    );

  }
}
