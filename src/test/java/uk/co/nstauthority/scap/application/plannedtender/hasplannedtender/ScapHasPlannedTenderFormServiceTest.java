package uk.co.nstauthority.scap.application.plannedtender.hasplannedtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
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
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
public class ScapHasPlannedTenderFormServiceTest {

  @Mock
  ScapHasPlannedTenderFormValidator scapHasPlannedTenderFormValidator;

  @Mock
  ScapPlannedTenderService scapPlannedTenderService;

  @Mock
  ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @InjectMocks
  ScapHasPlannedTenderFormService scapHasPlannedTenderFormService;

  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapDetail = new ScapDetail(null, 1, true, ScapDetailStatus.DRAFT, EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
  }

  @Test
  public void validate_verifyCallsValidator() {
    var form = new ScapHasPlannedTenderForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    scapHasPlannedTenderFormService.validate(form, bindingResult);

    verify(scapHasPlannedTenderFormValidator, times(1)).validate(form, bindingResult);
  }

  @Test
  public void getForm_noExistingPlannedTender() {
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());

    var form = scapHasPlannedTenderFormService.getForm(scapDetail);

    assertThat(form.getHasPlannedTender()).isNull();
  }

  @Test
  public void getForm_existingPlannedTender_yesPlannedActivity() {
    var existingPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    existingPlannedTender.setHasPlannedTenders(true);

    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingPlannedTender));

    var form = scapHasPlannedTenderFormService.getForm(scapDetail);

    assertThat(form.getHasPlannedTender()).isEqualTo(YesNo.YES);
  }

  @Test
  public void getForm_existingPlannedTender_noPlannedActivity() {
    var existingPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    existingPlannedTender.setHasPlannedTenders(false);

    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(existingPlannedTender));

    var form = scapHasPlannedTenderFormService.getForm(scapDetail);

    assertThat(form.getHasPlannedTender()).isEqualTo(YesNo.NO);
  }
}
