package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.fivium.energyportalapi.generated.types.Facility;
import uk.co.fivium.energyportalapi.generated.types.Field;
import uk.co.nstauthority.scap.energyportal.FacilityService;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsFormValidatorTest {

  @Mock
  FieldService fieldService;

  @Mock
  FacilityService facilityService;

  @InjectMocks
  ProjectDetailsFormValidator validator;

  private ProjectDetailsForm form;
  private BindingResult bindingResult;
  private static final List<Integer> VALID_FIELD_IDS = Collections.singletonList(33);
  private static final List<Field> FIELDS = Collections.singletonList(mock(Field.class));

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
  void validate_allFormFieldsMissing_assertHasExpectedErrors() {
    var emptyForm = new ProjectDetailsForm();
    emptyForm.setFieldIds(Collections.emptySet());
    var emptyFormBindingResult = new BeanPropertyBindingResult(emptyForm, "form");

    validator.validate(emptyForm, emptyFormBindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(emptyFormBindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("projectName.inputValue", Set.of("projectName.required")),
        entry("projectSummary.inputValue", Set.of("projectSummary.required")),
        entry("projectTypes", Set.of("projectTypes.required")),
        entry("projectCostEstimate.inputValue", Set.of("projectCostEstimate.required")),
        entry("awareOfLocalContentCommitment", Set.of("required")),
        entry("expectsToMeetLocalContentCommitment", Set.of("required")),
        entry("fieldSelector", Set.of("fieldSelector.required")),
        entry("hasPlatforms", Set.of("hasPlatforms.required")),
        entry("expectedStartDate.dayInput.inputValue", Set.of("expectedStartDate.dayInput.required")),
        entry("expectedStartDate.monthInput.inputValue", Set.of("expectedStartDate.monthInput.required")),
        entry("expectedStartDate.yearInput.inputValue", Set.of("expectedStartDate.yearInput.required")),
        entry("expectedEndDate.dayInput.inputValue", Set.of("expectedEndDate.dayInput.required")),
        entry("expectedEndDate.monthInput.inputValue", Set.of("expectedEndDate.monthInput.required")),
        entry("expectedEndDate.yearInput.inputValue", Set.of("expectedEndDate.yearInput.required"))
    );
  }

  @Test
  void validate_tooManyDecimalPlaces_assertHasExpectedErrors() {
    form.setProjectCostEstimate("0.1234");

    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("projectCostEstimate.inputValue", Set.of("projectCostEstimate.maxDecimalPlacesExceeded"))
    );
  }

  @Test
  void validate_ProjectSummaryMaxCount() {
    form.setProjectSummary("T".repeat(5000));
    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);
    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);
    assertThat(extractedErrors).containsExactly(
        entry("projectSummary.inputValue", Set.of("projectSummary.maxCharExceeded"))
    );
  }

  @Test
  void validate_tooSmallInputs_assertHasExpectedErrors() {
    form.setProjectCostEstimate("0");
    form.setExpectsToMeetLocalContentCommitment(true);

    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("projectCostEstimate.inputValue", Set.of("projectCostEstimate.minValueNotMet"))
    );
  }

  @Test
  void validate_fieldsInvalid_assertHasExpectedError() {
    var invalidFieldIds = Collections.singleton(9999);
    form.setFieldIds(invalidFieldIds);

    when(fieldService.getFieldsByIds(List.copyOf(invalidFieldIds), ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(Collections.emptyList());

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("fieldSelector", Set.of("fieldSelector.invalid"))
    );
  }

  @Test
  void validate_WillNotMeetCommitment_NoRationale_AssertError() {
    form.setExpectsToMeetLocalContentCommitment(false);
    form.setWillMissLocalContentCommitmentRationale(null);

    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(
            "willMissLocalContentCommitmentRationale.inputValue",
            Set.of("willMissLocalContentCommitmentRationale.required")
        )
    );
  }

  @Test
  void validate_startDateAfterEndDate_assertHasExpectedError() {
    form.setExpectedStartDate(LocalDate.now());
    form.setExpectedEndDate(LocalDate.now().minus(1, ChronoUnit.YEARS));

    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("expectedEndDate.dayInput.inputValue", Set.of("expectedEndDate.dayInput.minDateExclusiveNotMet")),
        entry("expectedEndDate.monthInput.inputValue", Set.of("expectedEndDate.monthInput.minDateExclusiveNotMet")),
        entry("expectedEndDate.yearInput.inputValue", Set.of("expectedEndDate.yearInput.minDateExclusiveNotMet"))
    );
  }

  @Test
  void validate_HasPlatforms_NoneSelected_AssertError() {
    form.setHasPlatforms(true);
    form.setInstallationIds(Collections.emptySet());

    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);
    var fieldName = ProjectDetailsFormValidator.INSTALLATION_SELECTOR_FIELD_NAME;

    assertThat(extractedErrors).containsExactly(
        entry(fieldName, Set.of("%s.required".formatted(fieldName)))
    );
  }

  @Test
  void validate_HasPlatforms_InvalidSelectedPlatforms() {
    var facilityIds = Collections.singleton(1209);

    form.setHasPlatforms(true);
    form.setInstallationIds(facilityIds);

    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);
    when(facilityService.findFacilitiesByIds(List.copyOf(facilityIds), ProjectDetailsFormValidator.INSTALLATIONS_REQUEST_PURPOSE))
        .thenReturn(Collections.emptyList());

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);
    var fieldName = ProjectDetailsFormValidator.INSTALLATION_SELECTOR_FIELD_NAME;

    assertThat(extractedErrors).containsExactly(
        entry(fieldName, Set.of("%s.invalid".formatted(fieldName)))
    );
  }

  @Test
  void validate_HasPlatforms() {
    var facilityId = 1209;
    var facilityIds = Collections.singleton(facilityId);
    var facilities = List.of(new Facility(facilityId, "Test facility", null, null, null));

    form.setHasPlatforms(true);
    form.setInstallationIds(facilityIds);

    when(fieldService.getFieldsByIds(VALID_FIELD_IDS, ProjectDetailsFormValidator.FIELDS_REQUEST_PURPOSE))
        .thenReturn(FIELDS);
    when(facilityService.findFacilitiesByIds(List.copyOf(facilityIds), ProjectDetailsFormValidator.INSTALLATIONS_REQUEST_PURPOSE))
        .thenReturn(facilities);

    validator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }



  private ProjectDetailsForm getValidProjectDetailsForm() {
    var form = new ProjectDetailsForm();
    form.setProjectName("Test project name");
    form.setProjectSummary("This is a test project");
    form.setProjectTypes(Set.of(ProjectType.CARBON_STORAGE_PERMIT));
    form.setProjectCostEstimate("2.2");
    form.setAwareOfLocalContentCommitment(true);
    form.setExpectsToMeetLocalContentCommitment(true);
    form.setFieldIds(new HashSet<>(VALID_FIELD_IDS));
    form.setHasPlatforms(false);
    form.setExpectedStartDate(LocalDate.of(2022, 1, 22));
    form.setExpectedEndDate(LocalDate.of(2023, 12, 27));
    return form;
  }
}
