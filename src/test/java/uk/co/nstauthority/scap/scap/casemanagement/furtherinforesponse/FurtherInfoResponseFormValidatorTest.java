package uk.co.nstauthority.scap.scap.casemanagement.furtherinforesponse;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.scap.casemanagement.consultationrequest.ConsultationRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.furtherinfo.FurtherInfoRequestForm;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentForm;
import uk.co.nstauthority.scap.scap.casemanagement.qacomments.QaCommentFormValidator;

@ExtendWith(MockitoExtension.class)
class FurtherInfoResponseFormValidatorTest {

  private FurtherInfoResponseForm form;

  private BindingResult bindingResult;

  @InjectMocks
  FurtherInfoResponseFormValidator validator;

  @Test
  void infoResponseFormValidator_Supports() {
    assertTrue(validator.supports(FurtherInfoResponseForm.class));
    assertFalse(validator.supports(FurtherInfoRequestForm.class));
  }

  @Test
  void furtherInfoValidation_commentsNotPresent() {
    form = new FurtherInfoResponseForm();
    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("infoResponse.inputValue").getDefaultMessage())
        .isEqualTo("Enter Response comments");
  }

  @Test
  void furtherInfoValidation_isValid() {
    form = new FurtherInfoResponseForm();

    var input = form.getInfoResponse();
    input.setInputValue("This is a Test String");
    form.setInfoResponse(input);
    form.setInfoResponseDocuments(Collections.singletonList(new FileUploadForm()));

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertFalse(bindingResult.hasFieldErrors());
  }

  @Test
  void scapApprovalFormValidator_maxCharacter_invalid() {
    form = new FurtherInfoResponseForm();
    var input = form.getInfoResponse();
    input.setInputValue("A".repeat(5000));
    form.setInfoResponse(input);

    bindingResult = new BeanPropertyBindingResult(form, "form");

    validator.validate(form, bindingResult);
    assertTrue(bindingResult.hasFieldErrors());
    assertThat(bindingResult.getFieldError("infoResponse.inputValue").getDefaultMessage())
        .isEqualTo("Response comments must be 4000 characters or less");
  }
}
