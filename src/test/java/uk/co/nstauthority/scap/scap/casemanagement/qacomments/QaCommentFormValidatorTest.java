package uk.co.nstauthority.scap.scap.casemanagement.qacomments;

import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;

@ExtendWith(MockitoExtension.class)
class QaCommentFormValidatorTest {

  private QaCommentForm form;

  private BindingResult bindingResult;

  @InjectMocks
  QaCommentFormValidator validator;

  @Test
  void furtherInfoValidation_commentsNotPresent() {
    form = new QaCommentForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }

  @Test
  void furtherInfoValidation_isValid() {
    form = new QaCommentForm();

    var input = form.getQaComments();
    input.setInputValue("This is a Test String");
    form.setQaComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }
}
