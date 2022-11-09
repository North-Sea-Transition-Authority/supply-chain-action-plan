package uk.co.nstauthority.scap.scap.actualtender.hasactualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@ExtendWith(MockitoExtension.class)
class HasActualTenderFormServiceTest {

  @Mock
  HasActualTenderFormValidator validator;

  @InjectMocks
  HasActualTenderFormService hasActualTenderFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new HasActualTenderForm();
    var inputBindingResult = new BeanPropertyBindingResult(form, "form");

    var outputBindingResult = hasActualTenderFormService.validate(form, inputBindingResult);

    assertThat(outputBindingResult).isEqualTo(inputBindingResult);
    verify(validator).validate(form, inputBindingResult);
  }

  @Test
  void getForm_nullHasPlannedTenders_assertEmptyForm() {
    var actualTender = new ActualTender();

    assertThat(hasActualTenderFormService.getForm(actualTender).getHasActualTender()).isNull();
  }

  @Test
  void getForm_yesHasPlannedTenders_assertFilledForm() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(true);

    assertThat(hasActualTenderFormService.getForm(actualTender).getHasActualTender()).isEqualTo(YesNo.YES);
  }

  @Test
  void getForm_noHasPlannedTenders_assertFilledForm() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(false);

    assertThat(hasActualTenderFormService.getForm(actualTender).getHasActualTender()).isEqualTo(YesNo.NO);
  }
}
