package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsFormValidatorTest {

  @Mock
  FieldService fieldService;

  @InjectMocks
  ProjectDetailsFormValidator validator;

  private ProjectDetailsForm form;
  private BindingResult bindingResult;
  private static final Integer VALID_FIELD_ID = 33;

  @BeforeEach
  void setup() {
    form = getValidProjectDetailsForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_projectDetailsForm_assertTrue() {
    assertTrue(validator.supports(ProjectDetailsForm.class));
  }

  @Test
  void supports_notSupportedClass_assertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_allFieldsMissing_assertHasExpectedErrors() {
    var emptyForm = new ProjectDetailsForm();
    var emptyFormBindingResult = new BeanPropertyBindingResult(emptyForm, "form");

    validator.validate(emptyForm, emptyFormBindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(emptyFormBindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("projectName.inputValue", Set.of("projectName.required")),
        entry("projectTypes", Set.of("projectTypes.required")),
        entry("projectCostEstimate.inputValue", Set.of("projectCostEstimate.required")),
        entry("estimatedValueLocalContent.inputValue", Set.of("estimatedValueLocalContent.required")),
        entry("fieldId.inputValue", Set.of("fieldId.required")),
        entry("startDay.inputValue", Set.of("startDay.required")),
        entry("startMonth.inputValue", Set.of("startMonth.required")),
        entry("startYear.inputValue", Set.of("startYear.required")),
        entry("endDay.inputValue", Set.of("endDay.required")),
        entry("endMonth.inputValue", Set.of("endMonth.required")),
        entry("endYear.inputValue", Set.of("endYear.required"))
    );
  }

  @Disabled("DFL currently only validates one field at a time")
  @Test
  void validate_tooManyDecimalPlaces_assertHasExpectedErrors() {
    form.setProjectCostEstimate("0.1234");
    form.setEstimatedValueLocalContent("0.1234");

    when(fieldService.doesFieldExist(VALID_FIELD_ID)).thenReturn(true);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("projectCostEstimate.inputValue", Set.of("projectCostEstimate.maxDecimalPlacesExceeded")),
        entry("estimatedValueLocalContent.inputValue", Set.of("estimatedValueLocalContent.maxDecimalPlacesExceeded"))
    );
  }

  @Disabled("DFL currently only validates one field at a time")
  @Test
  void validate_tooSmallInputs_assertHasExpectedErrors() {
    form.setProjectCostEstimate("0");
    form.setEstimatedValueLocalContent("0");

    when(fieldService.doesFieldExist(VALID_FIELD_ID)).thenReturn(true);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("projectCostEstimate.inputValue", Set.of("projectCostEstimate.minValueNotMet")),
        entry("estimatedValueLocalContent.inputValue", Set.of("estimatedValueLocalContent.minValueNotMet"))
    );
  }

  @Test
  void validate_localContentGreaterThanProjectCost_assertHasExpectedError() {
    form.setProjectCostEstimate("12.2");
    form.setEstimatedValueLocalContent("100");

    when(fieldService.doesFieldExist(VALID_FIELD_ID)).thenReturn(true);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("estimatedValueLocalContent.inputValue", Set.of("estimatedValueLocalContent.maxValueExceeded"))
    );
  }

  @Test
  void validate_fieldDoesNotExist_assertHasExpectedError() {
    var invalidFieldId = 9999;
    form.setFieldId(String.valueOf(invalidFieldId));

    when(fieldService.doesFieldExist(invalidFieldId)).thenReturn(false);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("fieldId", Set.of("fieldId.doesNotExist"))
    );
  }

  @Test
  void validate_startDateAfterEndDate_assertHasExpectedError() {
    form.setStartYear("2023");
    form.setEndYear("2022");

    when(fieldService.doesFieldExist(VALID_FIELD_ID)).thenReturn(true);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("endDay.inputValue", Set.of("endDay.minDateNotMet")),
        entry("endMonth.inputValue", Set.of("endMonth.minDateNotMet")),
        entry("endYear.inputValue", Set.of("endYear.minDateNotMet"))
    );
  }

  private ProjectDetailsForm getValidProjectDetailsForm() {
    var form = new ProjectDetailsForm();
    form.setProjectName("Test project name");
    form.setProjectTypes(Set.of(ProjectType.CARBON_STORAGE_PERMIT));
    form.setProjectCostEstimate("2.2");
    form.setEstimatedValueLocalContent("1.1");
    form.setFieldId(String.valueOf(VALID_FIELD_ID));
    form.setStartDay("22");
    form.setStartMonth("1");
    form.setStartYear("2022");
    form.setEndDay("27");
    form.setEndMonth("12");
    form.setEndYear("2023");
    return form;
  }
}
