package uk.co.nstauthority.scap.application.plannedtender.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ScapPlannedTenderDetailFormValidatorTest {

  private ScapPlannedTenderDetailFormValidator validator;
  private ScapPlannedTenderDetailForm form;
  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    validator = new ScapPlannedTenderDetailFormValidator();
    form = ScapPlannedTenderDetailFormTestUtil.getValidPlannedTenderDetailForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_hasPlannedTenderForm_assertTrue() {
    var formClass = ScapPlannedTenderDetailForm.class;
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
    var emptyForm = new ScapPlannedTenderDetailForm();
    var emptyFormBindingResult = new BeanPropertyBindingResult(emptyForm, "form");
    validator.validate(emptyForm, emptyFormBindingResult);
    var extractedErrors = ValidatorTestingUtil.extractErrors(emptyFormBindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("scopeDescription.inputValue", Set.of("scopeDescription.required")),
        entry("estimatedValue.inputValue", Set.of("estimatedValue.required")),
        entry("remunerationModel", Set.of("remunerationModel.required")),
        entry("awardRationale.inputValue", Set.of("awardRationale.required"))
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
