package uk.co.nstauthority.scap.scap.submit;

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
class ReviewAndSubmitFormValidatorTest {

  @InjectMocks
  ReviewAndSubmitFormValidator validator;

  private ReviewAndSubmitForm form;
  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    form = new ReviewAndSubmitForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");
  }

  @Test
  void supports_ReviewAndSubmitForm_AssertTrue() {
    assertTrue(validator.supports(ReviewAndSubmitForm.class));
  }

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(validator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void validate_WhenEmptyForm_AssertErrors() {
    var fieldName = ReviewAndSubmitFormValidator.APPROVED_BY_STAKEHOLDERS_FIELD;

    validator.validate(form, bindingResult);

    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(fieldName, Set.of("%s.required".formatted(fieldName)))
    );
  }

  @Test
  void validate_WhenIsReviewedByStakeholders_AssertNoErrors() {
    form.setApprovedByStakeholders(true);

    validator.validate(form, bindingResult);

    assertFalse(bindingResult.hasErrors());
  }
}
