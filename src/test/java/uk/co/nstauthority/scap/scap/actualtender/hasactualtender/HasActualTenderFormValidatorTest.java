package uk.co.nstauthority.scap.scap.actualtender.hasactualtender;

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
class HasActualTenderFormValidatorTest {

  @InjectMocks
  HasActualTenderFormValidator validator;

  @Test
  void supports_supportedClass_assertTrue() {
    assertTrue(validator.supports(HasActualTenderForm.class));
  }

  @Test
  void supports_nonSupportedClass_assertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_validForm_assertHasNoErrors() {
    var validForm = new HasActualTenderForm();
    validForm.setHasActualTender(YesNo.YES);
    var bindingResult = new BeanPropertyBindingResult(validForm, "validForm");

    validator.validate(validForm, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_emptyForm_assertHasError() {
    var emptyForm = new HasActualTenderForm();
    var bindingResult = new BeanPropertyBindingResult(emptyForm, "validForm");

    validator.validate(emptyForm, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("hasActualTender", Set.of("hasActualTender.required"))
    );
  }
}
