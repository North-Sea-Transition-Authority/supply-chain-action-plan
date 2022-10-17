package uk.co.nstauthority.scap.application.plannedtender.hasplannedtender;

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
public class ScapHasPlannedTenderFormValidatorTest {

  private ScapHasPlannedTenderFormValidator scapHasPlannedTenderFormValidator;
  private ScapHasPlannedTenderForm scapHasPlannedTenderForm;
  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    scapHasPlannedTenderFormValidator = new ScapHasPlannedTenderFormValidator();
    scapHasPlannedTenderForm = new ScapHasPlannedTenderForm();
    bindingResult = new BeanPropertyBindingResult(scapHasPlannedTenderForm, "form");
  }

  @Test
  public void supports_hasPlannedTenderForm_assertTrue() {
    var formClass = ScapHasPlannedTenderForm.class;
    assertTrue(scapHasPlannedTenderFormValidator.supports(formClass));
  }

  @Test
  public void supports_notSupportedClass_assertFalse() {
    var notSupportedClass = ValidatorTestingUtil.NonSupportedClass.class;
    assertFalse(scapHasPlannedTenderFormValidator.supports(notSupportedClass));
  }

  @Test
  public void validate_nullHasPlannedTender_assertPresenceError() {
    scapHasPlannedTenderForm.setHasPlannedTender(null);

    scapHasPlannedTenderFormValidator.validate(scapHasPlannedTenderForm, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("hasPlannedTender", Set.of("hasPlannedTender.presence"))
    );
  }

  @Test
  public void validate_validHasPlannedTender_assertHasNoErrors() {
    scapHasPlannedTenderForm.setHasPlannedTender(YesNo.YES);

    scapHasPlannedTenderFormValidator.validate(scapHasPlannedTenderForm, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }
}
