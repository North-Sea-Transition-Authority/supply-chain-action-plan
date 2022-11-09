package uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender;

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
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class HasPlannedTenderFormValidatorTest {

  private HasPlannedTenderFormValidator hasPlannedTenderFormValidator;
  private HasPlannedTenderForm hasPlannedTenderForm;
  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    hasPlannedTenderFormValidator = new HasPlannedTenderFormValidator();
    hasPlannedTenderForm = new HasPlannedTenderForm();
    bindingResult = new BeanPropertyBindingResult(hasPlannedTenderForm, "form");
  }

  @Test
  void supports_hasPlannedTenderForm_assertTrue() {
    var formClass = HasPlannedTenderForm.class;
    assertTrue(hasPlannedTenderFormValidator.supports(formClass));
  }

  @Test
  void supports_notSupportedClass_assertFalse() {
    var notSupportedClass = ValidatorTestingUtil.NonSupportedClass.class;
    assertFalse(hasPlannedTenderFormValidator.supports(notSupportedClass));
  }

  @Test
  void validate_nullHasPlannedTender_assertPresenceError() {
    hasPlannedTenderForm.setHasPlannedTender(null);

    hasPlannedTenderFormValidator.validate(hasPlannedTenderForm, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("hasPlannedTender", Set.of("hasPlannedTender.presence"))
    );
  }

  @Test
  void validate_validHasPlannedTender_assertHasNoErrors() {
    hasPlannedTenderForm.setHasPlannedTender(YesNo.YES);

    hasPlannedTenderFormValidator.validate(hasPlannedTenderForm, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }
}
