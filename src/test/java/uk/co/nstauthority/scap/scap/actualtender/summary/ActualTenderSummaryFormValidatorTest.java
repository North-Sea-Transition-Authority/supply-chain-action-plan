package uk.co.nstauthority.scap.scap.actualtender.summary;

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
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ActualTenderSummaryFormValidatorTest {

  @InjectMocks
  ActualTenderSummaryFormValidator validator;

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void supports_ActualTenderSummaryForm_AssertTrue() {
    assertTrue(validator.supports(ActualTenderSummaryForm.class));
  }

  @Test
  void validate_ValidForm_AssertNoErrors() {
    var form = new ActualTenderSummaryForm();
    form.setHasMoreActualTenderActivities(HasMoreActualTenderActivities.YES_LATER);
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }

  @Test
  void validate_MissingHasMoreActualTenderActivities() {
    var form = new ActualTenderSummaryForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry("hasMoreActualTenderActivities", Set.of("hasMoreActualTenderActivities.required"))
    );
  }
}
