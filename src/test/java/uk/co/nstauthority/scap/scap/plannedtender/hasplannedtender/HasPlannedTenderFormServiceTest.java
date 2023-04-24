package uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class HasPlannedTenderFormServiceTest {

  @Mock
  HasPlannedTenderFormValidator hasPlannedTenderFormValidator;

  @Mock
  PlannedTenderService plannedTenderService;

  @Mock
  PlannedTenderActivityService plannedTenderActivityService;

  @InjectMocks
  HasPlannedTenderFormService hasPlannedTenderFormService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail(null, 1, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
  }

  @Test
  void validate_verifyCallsValidator() {
    var form = new HasPlannedTenderForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    hasPlannedTenderFormService.validate(form, bindingResult);

    verify(hasPlannedTenderFormValidator).validate(form, bindingResult);
  }

  @Test
  void getForm_noExistingPlannedTender() {
    when(plannedTenderService.findByScapDetail(scapDetail)).thenReturn(Optional.empty());

    var form = hasPlannedTenderFormService.getForm(scapDetail);

    assertThat(form.getHasPlannedTender()).isNull();
  }

  @Test
  void getForm_existingPlannedTender_yesPlannedActivity() {
    var existingPlannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    existingPlannedTender.setHasPlannedTenders(true);

    when(plannedTenderService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingPlannedTender));

    var form = hasPlannedTenderFormService.getForm(scapDetail);

    assertThat(form.getHasPlannedTender()).isEqualTo(YesNo.YES);
  }

  @Test
  void getForm_existingPlannedTender_noPlannedActivity() {
    var existingPlannedTender = new PlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    existingPlannedTender.setHasPlannedTenders(false);

    when(plannedTenderService.findByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingPlannedTender));

    var form = hasPlannedTenderFormService.getForm(scapDetail);

    assertThat(form.getHasPlannedTender()).isEqualTo(YesNo.NO);
  }
}
