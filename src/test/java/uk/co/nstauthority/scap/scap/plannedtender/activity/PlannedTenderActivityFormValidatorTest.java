package uk.co.nstauthority.scap.scap.plannedtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDate;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class PlannedTenderActivityFormValidatorTest {

  private PlannedTenderActivityFormValidator validator;
  private PlannedTenderActivityForm form;
  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    validator = new PlannedTenderActivityFormValidator();
    form = ScapPlannedTenderDetailFormTestUtil.getValidPlannedTenderDetailForm();
    form.setIndicativeActualTenderStartDate(LocalDate.of(2000, 1, 1));
    form.setIndicativeContractAwardDate(LocalDate.of(2000, 2, 1));
    bindingResult = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_hasPlannedTenderForm_assertTrue() {
    var formClass = PlannedTenderActivityForm.class;
    assertTrue(validator.supports(formClass));
  }

  @Test
  void supports_notSupportedClass_assertFalse() {
    var notSupportedClass = ValidatorTestingUtil.NonSupportedClass.class;
    assertFalse(validator.supports(notSupportedClass));
  }

  @Test
  void validate_simpleValidForm_assertNoErrors() {
    validator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_emptyForm_assertPresenceErrors() {
    var emptyForm = new PlannedTenderActivityForm();
    var emptyFormBindingResult = new BeanPropertyBindingResult(emptyForm, "form");
    validator.validate(emptyForm, emptyFormBindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(emptyFormBindingResult);
    var startDateField = PlannedTenderActivityForm.INDICATIVE_ACTUAL_TENDER_START_DATE_FIELD;
    var awardDateField = PlannedTenderActivityForm.INDICATIVE_CONTRACT_AWARD_DATE_FIELD;

    assertThat(extractedErrors).containsExactly(
        entry("scopeDescription.inputValue", Set.of("scopeDescription.required")),
        entry("estimatedValue.inputValue", Set.of("estimatedValue.required")),
        entry("remunerationModel", Set.of("remunerationModel.required")),
        entry("awardRationale.inputValue", Set.of("awardRationale.required")),
        entry("%s.dayInput.inputValue".formatted(startDateField), Set.of("%s.dayInput.required".formatted(startDateField))),
        entry("%s.monthInput.inputValue".formatted(startDateField), Set.of("%s.monthInput.required".formatted(startDateField))),
        entry("%s.yearInput.inputValue".formatted(startDateField), Set.of("%s.yearInput.required".formatted(startDateField))),
        entry("%s.dayInput.inputValue".formatted(awardDateField), Set.of("%s.dayInput.required".formatted(awardDateField))),
        entry("%s.monthInput.inputValue".formatted(awardDateField), Set.of("%s.monthInput.required".formatted(awardDateField))),
        entry("%s.yearInput.inputValue".formatted(awardDateField), Set.of("%s.yearInput.required".formatted(awardDateField)))
    );
  }

  @Test
  void validate_estimatedValueTooSmall_assertError() {
    form.setEstimatedValue("0");

    validator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("estimatedValue.inputValue", Set.of("estimatedValue.minValueNotMet"))
    );
  }

  @Test
  void validate_estimatedValueTooManyDecimalPlaces_assertError() {
    form.setEstimatedValue("0.1234");

    validator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("estimatedValue.inputValue", Set.of("estimatedValue.maxDecimalPlacesExceeded"))
    );
  }

  @Test
  void validate_validOtherRemunerationModel_assertNoErrors() {
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Test other remuneration model");

    validator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_missingRemunerationModelName_assertPresenceError() {
    form.setRemunerationModel(RemunerationModel.OTHER);

    validator.validate(form, bindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("remunerationModelName.inputValue", Set.of("remunerationModelName.required"))
    );
  }

}
