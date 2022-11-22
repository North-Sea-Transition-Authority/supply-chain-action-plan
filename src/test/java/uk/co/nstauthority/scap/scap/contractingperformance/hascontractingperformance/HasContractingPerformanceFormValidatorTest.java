package uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class HasContractingPerformanceFormValidatorTest {

  @InjectMocks
  HasContractingPerformanceFormValidator validator;

  @Test
  void supports_HasContractingPerformanceForm_AssertTrue() {
    assertTrue(validator.supports(HasContractingPerformanceForm.class));
  }

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_EmptyForm_AssertError() {
    var form = new HasContractingPerformanceForm();
    var errors = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, errors);
    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry("hasContractingPerformance", Set.of("hasContractingPerformance.required"))
    );
  }

  @Test
  void validate_ValidForm_AssertNoErrors() {
    var form = new HasContractingPerformanceForm();
    form.setHasContractingPerformance(YesNo.NO);
    var errors = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, errors);

    assertFalse(errors.hasErrors());
  }
}
