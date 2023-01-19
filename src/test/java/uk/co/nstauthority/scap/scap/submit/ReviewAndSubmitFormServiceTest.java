package uk.co.nstauthority.scap.scap.submit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
class ReviewAndSubmitFormServiceTest {

  @Mock
  ReviewAndSubmitFormValidator reviewAndSubmitFormValidator;

  @InjectMocks
  ReviewAndSubmitFormService reviewAndSubmitFormService;

  @Test
  void validate_VerifyCallsValidator() {
    var form = new ReviewAndSubmitForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    reviewAndSubmitFormService.validate(form, bindingResult);

    verify(reviewAndSubmitFormValidator).validate(form, bindingResult);
  }

  @Test
  void getForm() {
    var scapDetail = new ScapDetail();
    scapDetail.setApprovedByStakeholders(true);

    var form = reviewAndSubmitFormService.getForm(scapDetail);

    assertThat(form.getApprovedByStakeholders()).isEqualTo(scapDetail.getApprovedByStakeholders());
  }
}
