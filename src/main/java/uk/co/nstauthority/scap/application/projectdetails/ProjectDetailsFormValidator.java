package uk.co.nstauthority.scap.application.projectdetails;

import java.math.BigDecimal;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.date.ThreeFieldDateInputValidator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.fivium.formlibrary.validator.integer.IntegerInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.energyportal.FieldService;

@Service
class ProjectDetailsFormValidator implements Validator {

  private final FieldService fieldService;

  @Autowired
  public ProjectDetailsFormValidator(FieldService fieldService) {
    this.fieldService = fieldService;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return ProjectDetailsForm.class.equals(clazz);
  }

  @Override
  public void validate(@NotNull Object target, @NotNull Errors errors) {
    var form = (ProjectDetailsForm) target;

    StringInputValidator.builder().validate(form.getProjectName(), errors);

    ValidationUtils.rejectIfEmpty(errors,
        "projectTypes",
        "projectTypes.required",
        "Select at least one project type"
    );

    DecimalInputValidator.builder()
        .mustHaveNoMoreThanDecimalPlaces(3)
        .mustBeMoreThanOrEqual(BigDecimal.valueOf(0.001))
        .validate(form.getProjectCostEstimate(), errors);

    var localContentValueValidatorBuilder = DecimalInputValidator.builder()
        .mustHaveNoMoreThanDecimalPlaces(3)
        .mustBeMoreThanOrEqual(BigDecimal.valueOf(0.001));
    var projectCostEstimate = form.getProjectCostEstimate().getAsBigDecimal();
    if (!form.getProjectCostEstimate().fieldHasErrors(errors) && projectCostEstimate.isPresent()) {
      localContentValueValidatorBuilder.mustBeLessThanOrEqualTo(projectCostEstimate.get());
    }
    localContentValueValidatorBuilder.validate(form.getEstimatedValueLocalContent(), errors);

    IntegerInputValidator.builder()
        .mustBeMoreThanOrEqual(0)
        .validate(form.getFieldId(), errors);

    if (!errors.hasFieldErrors("fieldId.inputValue")
        && !fieldService.doesFieldExist(Integer.valueOf(form.getFieldId().getInputValue()))) {
      errors.rejectValue("fieldId", "fieldId.doesNotExist", "Select a valid field");
    }

    ThreeFieldDateInputValidator.builder().validate(form.getExpectedStartDate(), errors);

    var startDate = form.getExpectedStartDate().getAsLocalDate();
    var endDateValidatorBuilder = ThreeFieldDateInputValidator.builder();
    if (startDate.isPresent()) {
      endDateValidatorBuilder = endDateValidatorBuilder.mustBeAfterDate(startDate.get());
    }
    endDateValidatorBuilder.validate(form.getExpectedEndDate(), errors);
  }
}
