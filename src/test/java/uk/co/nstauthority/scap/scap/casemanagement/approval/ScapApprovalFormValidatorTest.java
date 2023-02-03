package uk.co.nstauthority.scap.scap.casemanagement.approval;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentForm;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentFormValidator;

@ExtendWith(MockitoExtension.class)
class ScapApprovalFormValidatorTest {

  private ScapApprovalForm form;

  private BindingResult bindingResult;

  @InjectMocks
  ScapApprovalFormValidator validator;

  @Test
  void scapApprovalFormValidator_commentsNotPresent() {
    form = new ScapApprovalForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("approvalComments.inputValue").getDefaultMessage())
        .isEqualTo("Enter Approval comments");
  }

  @Test
  void scapApprovalFormValidator_isValid() {
    form = new ScapApprovalForm();

    var input = form.getApprovalComments();
    input.setInputValue("This is a Test String");
    form.setApprovalComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }

  @Test
  void scapApprovalFormValidator_maxCharacter_invalid() {
    form = new ScapApprovalForm();
    var input = form.getApprovalComments();
    input.setInputValue("A".repeat(5000));
    form.setApprovalComments(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("approvalComments.inputValue").getDefaultMessage())
        .isEqualTo("Approval comments must be 4000 characters or less");
  }

  @Test
  void scapApprovalFormValidator_Supports() {
    assertTrue(validator.supports(ScapApprovalForm.class));
    assertFalse(validator.supports(FurtherInfoRequestForm.class));
  }
}
