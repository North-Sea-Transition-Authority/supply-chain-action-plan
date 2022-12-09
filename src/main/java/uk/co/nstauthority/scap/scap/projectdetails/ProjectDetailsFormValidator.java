package uk.co.nstauthority.scap.scap.projectdetails;

import java.math.BigDecimal;
import java.util.Objects;
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
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.enumutil.YesNo;

@Service
class ProjectDetailsFormValidator implements Validator {

  static final String INSTALLATION_SELECTOR_FIELD_NAME = "installationSelector";
  static final String INSTALLATIONS_REQUEST_PURPOSE = "Verify facilities exist for SCAP project details validation";
  private final FieldService fieldService;
  private final FacilityService facilityService;

  @Autowired
  public ProjectDetailsFormValidator(FieldService fieldService, FacilityService facilityService) {
    this.fieldService = fieldService;
    this.facilityService = facilityService;
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

    ValidationUtils.rejectIfEmpty(
        errors,
        "hasPlatforms",
        "hasPlatforms.required",
        "Select whether there are any installations or subsea infrastructure related to this project"
    );

    if (YesNo.YES.equals(form.getHasPlatforms())) {

      if (form.getInstallationIds().isEmpty()) {
        errors.rejectValue(
            INSTALLATION_SELECTOR_FIELD_NAME,
            "%s.required".formatted(INSTALLATION_SELECTOR_FIELD_NAME),
            "Select at least one related installation"
        );
      }

      if (!errors.hasFieldErrors(INSTALLATION_SELECTOR_FIELD_NAME)) {
        var facilities = facilityService.findFacilitiesByIds(form.getInstallationIds(), INSTALLATIONS_REQUEST_PURPOSE);
        if (!Objects.equals(facilities.size(), form.getInstallationIds().size())) {
          errors.rejectValue(INSTALLATION_SELECTOR_FIELD_NAME,
              "%s.invalid".formatted(INSTALLATION_SELECTOR_FIELD_NAME),
              "Select valid installations only"
          );
        }
      }
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
