package uk.co.nstauthority.scap.scap.plannedtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class PlannedTenderFormServiceTest {

  @Mock
  PlannedTenderFormValidator plannedTenderFormValidator;

  @InjectMocks
  PlannedTenderFormService plannedTenderFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new PlannedTenderForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    var returnedBindingResult = plannedTenderFormService.validate(form, bindingResult);

    assertThat(returnedBindingResult).isEqualTo(bindingResult);
    verify(plannedTenderFormValidator).validate(form, bindingResult);
  }

  @Test
  void getForm_nullHasMorePlannedTenderActivities_assertEmptyForm() {
    var scapPlannedTender = new PlannedTender(null, EntityTestingUtil.dateToInstant(2000, 12, 30));

    var form = plannedTenderFormService.getForm(scapPlannedTender);

    assertThat(form.getHasMorePlannedTenderActivities()).isNull();
  }

  @Test
  void getForm_yesHasMorePlannedTenderActivities_assertEmptyForm() {
    var scapPlannedTender = new PlannedTender(null, EntityTestingUtil.dateToInstant(2000, 12, 30));
    scapPlannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_LATER);

    var form = plannedTenderFormService.getForm(scapPlannedTender);

    assertThat(form.getHasMorePlannedTenderActivities()).isNull();
  }

  @Test
  void getForm_noHasMorePlannedTenderActivities_assertFilledForm() {
    var scapPlannedTender = new PlannedTender(null, EntityTestingUtil.dateToInstant(2000, 12, 30));
    scapPlannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);

    var form = plannedTenderFormService.getForm(scapPlannedTender);

    assertThat(form.getHasMorePlannedTenderActivities()).isEqualTo(HasMorePlannedTenderActivities.NO);
  }
}
