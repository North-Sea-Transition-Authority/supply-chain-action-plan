package uk.co.nstauthority.scap.feedback;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.Collections;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.util.ValidationUtil;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class FeedbackFormValidatorTest {

  @InjectMocks
  private FeedbackFormValidator feedbackFormValidator;

  private FeedbackForm form;
  private BindingResult bindingResult;

  @BeforeEach
  void setup() {
    form = new FeedbackForm();
    bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);
  }

  @Test
  void validate_EmptyForm_AssertErrors() {
    var returnedBindingResult = feedbackFormValidator.validate(form, bindingResult);

    assertThat(ValidatorTestingUtil.extractErrors(returnedBindingResult))
        .containsExactly(
            entry(FeedbackForm.SATISFACTION_FIELD_NAME, Collections.singleton("required"))
        );
  }

  @Test
  void validate_ValidForm_AssertNoErrors() {
    form.setSatisfactionRating(SatisfactionRating.NEITHER);
    var returnedBindingResult = feedbackFormValidator.validate(form, bindingResult);

    assertFalse(returnedBindingResult.hasErrors());
  }

  @Test
  void validate_TooLongComments_AssertErrors() {
    form.setSatisfactionRating(SatisfactionRating.NEITHER);
    form.setComments("F".repeat(ValidationUtil.TEXT_AREA_STANDARD_LIMIT + 1));
    var returnedBindingResult = feedbackFormValidator.validate(form, bindingResult);

    var commentsFieldName = form.getComments().getFieldName();

    assertThat(ValidatorTestingUtil.extractErrors(returnedBindingResult))
        .containsExactly(
            entry(commentsFieldName+".inputValue", Collections.singleton(commentsFieldName+".maxCharExceeded"))
        );
  }

}
