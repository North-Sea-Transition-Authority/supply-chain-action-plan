package uk.co.nstauthority.scap.scap.plannedtender;

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
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class PlannedTenderFormValidatorTest {

  private PlannedTenderFormValidator plannedTenderFormValidator;

  @BeforeEach
  void setup() {
    plannedTenderFormValidator = new PlannedTenderFormValidator();
  }

  @Test
  void supports_supportedClass_assertTrue() {
    assertTrue(plannedTenderFormValidator.supports(PlannedTenderForm.class));
  }

  @Test
  void supports_notSupportedClass_assertFalse() {
    assertFalse(plannedTenderFormValidator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_nullHasMorePlannedTenderActivities_assertHasErrors() {
    var form = new PlannedTenderForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    plannedTenderFormValidator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("hasMorePlannedTenderActivities", Set.of("hasMorePlannedTenderActivities.required"))
    );
  }

  @Test
  void validate_validForm_assertNoErrors() {
    var form = new PlannedTenderForm();
    form.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    plannedTenderFormValidator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }
}
