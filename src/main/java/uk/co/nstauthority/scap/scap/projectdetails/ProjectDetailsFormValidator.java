package uk.co.nstauthority.scap.scap.projectdetails;

import java.math.BigDecimal;
import java.util.List;
import java.util.Objects;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.date.ThreeFieldDateInputValidator;
import uk.co.fivium.formlibrary.validator.decimal.DecimalInputValidator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.util.ValidationUtil;
import uk.co.nstauthority.scap.util.ValidatorErrorCodes;

@Service
class ProjectDetailsFormValidator implements Validator {

  static final String AWARE_OF_LOCAL_CONTENT_FIELD = "awareOfLocalContentCommitment";
  static final String NOT_AWARE_LOCAL_CONTENT_ERROR = """
      Ensure that you are aware of the industry voluntary commitment to achieving 50% UK content on all related \
      new energy and decommissioning projects""";
  static final String WILL_MEET_LOCAL_CONTENT_FIELD = "expectsToMeetLocalContentCommitment";
  static final String MISSING_WILL_MEET_LOCAL_CONTENT_ERROR = """
      Select whether you expect to meet the industry voluntary commitment to achieving 50% UK content on all related \
      new energy and decommissioning projects""";
  static final String INSTALLATION_SELECTOR_FIELD_NAME = "installationSelector";
  static final String INSTALLATIONS_REQUEST_PURPOSE = "Verify facilities exist for SCAP project details validation";
  static final String FIELDS_SELECTOR_FIELD_NAME = "fieldSelector";
  static final String FIELDS_REQUEST_PURPOSE = "Verify fields exist for SCAP project details";
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

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getProjectName(), errors);

    StringInputValidator.builder()
        .mustHaveCharacterCountAtMost(ValidationUtil.TEXT_AREA_STANDARD_LIMIT)
        .validate(form.getProjectSummary(), errors);

    ValidationUtils.rejectIfEmpty(errors,
        "projectTypes",
        "projectTypes.required",
        "Select at least one project type"
    );

    DecimalInputValidator.builder()
        .mustHaveNoMoreThanDecimalPlaces(3)
        .mustBeMoreThanOrEqualTo(BigDecimal.valueOf(0.001))
        .validate(form.getProjectCostEstimate(), errors);

    if (!Boolean.TRUE.equals(form.getAwareOfLocalContentCommitment())) {
      errors.rejectValue(
          AWARE_OF_LOCAL_CONTENT_FIELD,
          ValidatorErrorCodes.REQUIRED.getErrorCode(),
          NOT_AWARE_LOCAL_CONTENT_ERROR
      );
    }

    if (Objects.isNull(form.getExpectsToMeetLocalContentCommitment())) {
      errors.rejectValue(
          WILL_MEET_LOCAL_CONTENT_FIELD,
          ValidatorErrorCodes.REQUIRED.getErrorCode(),
          MISSING_WILL_MEET_LOCAL_CONTENT_ERROR
      );
    }

    if (Boolean.FALSE.equals(form.getExpectsToMeetLocalContentCommitment())) {
      StringInputValidator.builder().validate(form.getWillMissLocalContentCommitmentRationale(), errors);
    }

    if (form.getFieldIds().isEmpty()) {
      errors.rejectValue(
          FIELDS_SELECTOR_FIELD_NAME,
          "%s.required".formatted(FIELDS_SELECTOR_FIELD_NAME),
          "Select at least one field"
      );
    }

    if (!errors.hasFieldErrors(FIELDS_SELECTOR_FIELD_NAME)) {
      var fieldIds = form.getFieldIds();
      if (fieldIds.size() != fieldService.getFieldsByIds(List.copyOf(fieldIds), FIELDS_REQUEST_PURPOSE).size()) {
        errors.rejectValue(
            FIELDS_SELECTOR_FIELD_NAME,
            "%s.invalid".formatted(FIELDS_SELECTOR_FIELD_NAME),
            "Selected fields must all be valid");
      }
    }

    ValidationUtils.rejectIfEmpty(
        errors,
        "hasPlatforms",
        "hasPlatforms.required",
        "Select whether there are any installations or subsea infrastructure related to this project"
    );

    if (Boolean.TRUE.equals(form.getHasPlatforms())) {

      if (form.getInstallationIds().isEmpty()) {
        errors.rejectValue(
            INSTALLATION_SELECTOR_FIELD_NAME,
            "%s.required".formatted(INSTALLATION_SELECTOR_FIELD_NAME),
            "Select at least one related installation"
        );
      }

      if (!errors.hasFieldErrors(INSTALLATION_SELECTOR_FIELD_NAME)) {
        var facilities = facilityService
            .findFacilitiesByIds(List.copyOf(form.getInstallationIds()), INSTALLATIONS_REQUEST_PURPOSE);
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
