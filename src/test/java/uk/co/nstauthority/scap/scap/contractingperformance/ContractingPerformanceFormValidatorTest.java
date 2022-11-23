package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.Set;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceFormValidatorTest {

  @InjectMocks
  ContractingPerformanceFormValidator validator;

  ContractingPerformanceForm form;
  BindingResult errors;

  @BeforeEach
  void setup() {
    form = new ContractingPerformanceForm();
    errors = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_ContractingPerformanceForm_AssertTrue() {
    assertTrue(validator.supports(ContractingPerformanceForm.class));
  }

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_NoValidationHint_AssertThrows() {
    assertThatIllegalStateException().isThrownBy(() -> validator.validate(form, errors));
  }

  @Test
  void validate_EmptyForm_AssertErrors() {
    var validatorHint = new ContractingPerformanceFormValidatorHint(Collections.emptySet());

    validator.validate(form, errors, validatorHint);
    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("actualTenderActivityId", Set.of("actualTenderActivityId.required")),
        entry("outturnCost.inputValue", Set.of("outturnCost.required"))
    );
  }

  @Test
  void validate_ValidForm_AssertNoErrors() {
    var actualTenderActivityId = 49;
    var validatorHint = new ContractingPerformanceFormValidatorHint(Set.of(actualTenderActivityId));
    form.setActualTenderActivityId(actualTenderActivityId);
    form.setOutturnCost("15.5");
    form.setOutturnRationale("Some outturn rationale");

    validator.validate(form, errors, validatorHint);

    assertFalse(errors.hasErrors());
  }

  @Test
  void validate_NonExistentActualTenderActivity() {
    var validatorHint = new ContractingPerformanceFormValidatorHint(Collections.emptySet());
    form.setActualTenderActivityId(49);
    form.setOutturnCost("15.5");
    form.setOutturnRationale("Some outturn rationale");

    validator.validate(form, errors, validatorHint);
    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("actualTenderActivityId", Set.of("actualTenderActivityId.doesNotExist"))
    );
  }
}
