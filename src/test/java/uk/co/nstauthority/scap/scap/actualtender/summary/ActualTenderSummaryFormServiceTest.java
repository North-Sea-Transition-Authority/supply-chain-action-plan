package uk.co.nstauthority.scap.scap.actualtender.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@ExtendWith(MockitoExtension.class)
class ActualTenderSummaryFormServiceTest {

  @Mock
  ActualTenderSummaryFormValidator validator;

  @InjectMocks
  ActualTenderSummaryFormService actualTenderSummaryFormService;

  @Test
  void validate_VerifyCallsValidator() {
    var form = new ActualTenderSummaryForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = actualTenderSummaryFormService.validate(form, bindingResult);

    verify(validator).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
  }

  @Test
  void getForm_HasMorePlannedTenderToAdd_AssertEmpty() {
    var actualTender = new ActualTender(46);
    actualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.YES_LATER);
    var form = actualTenderSummaryFormService.getForm(actualTender);

    assertThat(form.getHasMoreActualTenderActivities()).isNull();
  }

  @Test
  void getForm_NoMorePlannedTenderToAdd_AssertAutofilled() {
    var actualTender = new ActualTender(46);
    actualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.NO);
    var form = actualTenderSummaryFormService.getForm(actualTender);

    assertThat(form.getHasMoreActualTenderActivities()).isEqualTo(actualTender.getHasMoreActualTenders());
  }
}
