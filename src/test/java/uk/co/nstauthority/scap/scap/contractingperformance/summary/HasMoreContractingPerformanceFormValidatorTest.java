package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
class HasMoreContractingPerformanceFormValidatorTest {

  @InjectMocks
  HasMoreContractingPerformanceFormValidator validator;

  private HasMoreContractingPerformanceForm form;
  private BindingResult errors;

  @BeforeEach
  void setup() {
    form = new HasMoreContractingPerformanceForm();
    errors = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_HasMoreContractingPerformanceForm_AssertTrue() {
    assertTrue(validator.supports(HasMoreContractingPerformanceForm.class));
  }

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_EmptyForm_ExpectErrors() {
    validator.validate(form, errors);

    var extractedErrors = ValidatorTestingUtil.extractErrors(errors);

    assertThat(extractedErrors).containsExactly(
        entry(HasMoreContractingPerformanceFormValidator.HAS_MORE_CONTRACTING_PERFORMANCE_FIELD_NAME,
            Set.of("%s.required"
                .formatted(HasMoreContractingPerformanceFormValidator.HAS_MORE_CONTRACTING_PERFORMANCE_FIELD_NAME)))
    );
  }

  @Test
  void validate_ValidForm_NoErrors() {
    form.setHasMoreContractingPerformance(HasMoreContractingPerformance.NO);

    validator.validate(form, errors);

    assertFalse(errors.hasErrors());
  }
}
