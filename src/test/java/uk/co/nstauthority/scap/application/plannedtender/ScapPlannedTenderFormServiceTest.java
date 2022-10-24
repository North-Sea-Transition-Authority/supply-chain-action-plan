package uk.co.nstauthority.scap.application.plannedtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class ScapPlannedTenderFormServiceTest {

  @Mock
  ScapPlannedTenderFormValidator scapPlannedTenderFormValidator;

  @InjectMocks
  ScapPlannedTenderFormService scapPlannedTenderFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ScapPlannedTenderForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = scapPlannedTenderFormService.validate(form, bindingResult);

    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    verify(scapPlannedTenderFormValidator, times(1)).validate(form, bindingResult);
  }

  @Test
  void getForm_nullHasMorePlannedTenderActivities_assertEmptyForm() {
    var scapPlannedTender = new ScapPlannedTender(null, EntityTestingUtil.dateToInstant(2000, 12, 30));

    var form = scapPlannedTenderFormService.getForm(scapPlannedTender);

    assertThat(form.getHasMorePlannedTenderActivities()).isNull();
  }

  @Test
  void getForm_yesHasMorePlannedTenderActivities_assertEmptyForm() {
    var scapPlannedTender = new ScapPlannedTender(null, EntityTestingUtil.dateToInstant(2000, 12, 30));
    scapPlannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_LATER);

    var form = scapPlannedTenderFormService.getForm(scapPlannedTender);

    assertThat(form.getHasMorePlannedTenderActivities()).isNull();
  }

  @Test
  void getForm_noHasMorePlannedTenderActivities_assertFilledForm() {
    var scapPlannedTender = new ScapPlannedTender(null, EntityTestingUtil.dateToInstant(2000, 12, 30));
    scapPlannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);

    var form = scapPlannedTenderFormService.getForm(scapPlannedTender);

    assertThat(form.getHasMorePlannedTenderActivities()).isEqualTo(HasMorePlannedTenderActivities.NO);
  }
}
