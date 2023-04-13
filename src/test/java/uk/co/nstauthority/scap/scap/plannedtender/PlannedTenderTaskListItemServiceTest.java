package uk.co.nstauthority.scap.scap.plannedtender;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityForm;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityFormService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderForm;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderFormService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class PlannedTenderTaskListItemServiceTest {


  @Mock
  private PlannedTenderService plannedTenderService;

  @Mock
  private HasPlannedTenderFormService hasPlannedTenderFormService;

  @Mock
  private PlannedTenderActivityService plannedTenderActivityService;

  @Mock
  private PlannedTenderActivityFormService plannedTenderActivityFormService;

  @InjectMocks
  private PlannedTenderTaskListItemService plannedTenderTaskListItemService;

  private static final ScapId SCAP_ID = new ScapId(242);
  private static final Scap SCAP = ScapEntityTestUtil.scapBuilder()
      .withScapId(SCAP_ID)
      .build();
  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder()
      .withScap(SCAP)
      .build();

  @Test
  void isValid_NoPlannedTenderOverview_AssertFalse() {
    when(plannedTenderService.getScapPlannedTenderByScapDetail(SCAP_DETAIL)).thenReturn(Optional.empty());

    assertFalse(plannedTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        plannedTenderService,
        hasPlannedTenderFormService,
        plannedTenderActivityService,
        plannedTenderActivityFormService
    );
  }

  @Test
  void isValid_HasPlannedTenderActivitiesFormInvalid_AssertFalse() {
    var plannedTender = new PlannedTender();
    var hasPlannedTenderForm = new HasPlannedTenderForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithErrors(hasPlannedTenderForm);

    when(plannedTenderService.getScapPlannedTenderByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(plannedTender));
    when(hasPlannedTenderFormService.scapPlannedTenderToForm(plannedTender)).thenReturn(hasPlannedTenderForm);
    when(hasPlannedTenderFormService.validate(eq(hasPlannedTenderForm), any(BindingResult.class)))
        .thenReturn(bindingResult);

    assertFalse(plannedTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        plannedTenderService,
        hasPlannedTenderFormService,
        plannedTenderActivityService,
        plannedTenderActivityFormService
    );
  }

  @Test
  void isValid_HasNoPlannedTenderActivities_AssertTrue() {
    var plannedTender = new PlannedTender();
    var hasPlannedTenderForm = new HasPlannedTenderForm();
    hasPlannedTenderForm.setHasPlannedTender(YesNo.NO);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(hasPlannedTenderForm);

    when(plannedTenderService.getScapPlannedTenderByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(plannedTender));
    when(hasPlannedTenderFormService.scapPlannedTenderToForm(plannedTender)).thenReturn(hasPlannedTenderForm);
    when(hasPlannedTenderFormService.validate(eq(hasPlannedTenderForm), any(BindingResult.class)))
        .thenReturn(bindingResult);

    assertTrue(plannedTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        plannedTenderService,
        hasPlannedTenderFormService,
        plannedTenderActivityService,
        plannedTenderActivityFormService
    );
  }

  @Test
  void isValid_HasPlannedTenderActivities_NoneAdded_AssertFalse() {
    var plannedTender = new PlannedTender();
    var hasPlannedTenderForm = new HasPlannedTenderForm();
    hasPlannedTenderForm.setHasPlannedTender(YesNo.YES);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(hasPlannedTenderForm);

    when(plannedTenderService.getScapPlannedTenderByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(plannedTender));
    when(hasPlannedTenderFormService.scapPlannedTenderToForm(plannedTender)).thenReturn(hasPlannedTenderForm);
    when(hasPlannedTenderFormService.validate(eq(hasPlannedTenderForm), any(BindingResult.class)))
        .thenReturn(bindingResult);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(Collections.emptyList());

    assertFalse(plannedTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        plannedTenderService,
        hasPlannedTenderFormService,
        plannedTenderActivityService,
        plannedTenderActivityFormService
    );
  }

  @Test
  void isValid_InvalidPlannedTenderActivity_AssertFalse() {
    var plannedTender = new PlannedTender();
    var hasPlannedTenderForm = new HasPlannedTenderForm();
    hasPlannedTenderForm.setHasPlannedTender(YesNo.YES);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(hasPlannedTenderForm);
    var plannedTenderActivity = new PlannedTenderActivity();
    var plannedTenderActivities = Collections.singletonList(plannedTenderActivity);
    var plannedTenderActivityForm = new PlannedTenderActivityForm();
    var plannedTenderActivityErrors = ValidatorTestingUtil
        .bindingResultWithErrors(plannedTenderActivityForm);

    when(plannedTenderService.getScapPlannedTenderByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(plannedTender));
    when(hasPlannedTenderFormService.scapPlannedTenderToForm(plannedTender)).thenReturn(hasPlannedTenderForm);
    when(hasPlannedTenderFormService.validate(eq(hasPlannedTenderForm), any(BindingResult.class)))
        .thenReturn(bindingResult);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(plannedTenderActivities);
    when(plannedTenderActivityFormService.getForm(plannedTenderActivity))
        .thenReturn(plannedTenderActivityForm);
    when(plannedTenderActivityFormService.validate(any(BindingResult.class), eq(plannedTenderActivityForm)))
        .thenReturn(plannedTenderActivityErrors);

    assertFalse(plannedTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        plannedTenderService,
        hasPlannedTenderFormService,
        plannedTenderActivityService,
        plannedTenderActivityFormService
    );
  }

  @Test
  void isValid_HasMorePlannedTenderActivitiesToAdd_AssertFalse() {
    var plannedTender = new PlannedTender();
    plannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.YES_LATER);
    var hasPlannedTenderForm = new HasPlannedTenderForm();
    hasPlannedTenderForm.setHasPlannedTender(YesNo.YES);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(hasPlannedTenderForm);
    var plannedTenderActivity = new PlannedTenderActivity();
    var plannedTenderActivities = Collections.singletonList(plannedTenderActivity);
    var plannedTenderActivityForm = new PlannedTenderActivityForm();
    var plannedTenderActivityErrors = ValidatorTestingUtil
        .bindingResultWithoutErrors(plannedTenderActivityForm);

    when(plannedTenderService.getScapPlannedTenderByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(plannedTender));
    when(hasPlannedTenderFormService.scapPlannedTenderToForm(plannedTender)).thenReturn(hasPlannedTenderForm);
    when(hasPlannedTenderFormService.validate(eq(hasPlannedTenderForm), any(BindingResult.class)))
        .thenReturn(bindingResult);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(plannedTenderActivities);
    when(plannedTenderActivityFormService.getForm(plannedTenderActivity))
        .thenReturn(plannedTenderActivityForm);
    when(plannedTenderActivityFormService.validate(any(BindingResult.class), eq(plannedTenderActivityForm)))
        .thenReturn(plannedTenderActivityErrors);

    assertFalse(plannedTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        plannedTenderService,
        hasPlannedTenderFormService,
        plannedTenderActivityService,
        plannedTenderActivityFormService
    );
  }

  @Test
  void isValid_HasMorePlannedTenderActivitiesToAdd_AssertTrue() {
    var plannedTender = new PlannedTender();
    plannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);
    var hasPlannedTenderForm = new HasPlannedTenderForm();
    hasPlannedTenderForm.setHasPlannedTender(YesNo.YES);
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(hasPlannedTenderForm);
    var plannedTenderActivity = new PlannedTenderActivity();
    var plannedTenderActivities = Collections.singletonList(plannedTenderActivity);
    var plannedTenderActivityForm = new PlannedTenderActivityForm();
    var plannedTenderActivityErrors = ValidatorTestingUtil
        .bindingResultWithoutErrors(plannedTenderActivityForm);

    when(plannedTenderService.getScapPlannedTenderByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(plannedTender));
    when(hasPlannedTenderFormService.scapPlannedTenderToForm(plannedTender)).thenReturn(hasPlannedTenderForm);
    when(hasPlannedTenderFormService.validate(eq(hasPlannedTenderForm), any(BindingResult.class)))
        .thenReturn(bindingResult);
    when(plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender))
        .thenReturn(plannedTenderActivities);
    when(plannedTenderActivityFormService.getForm(plannedTenderActivity))
        .thenReturn(plannedTenderActivityForm);
    when(plannedTenderActivityFormService.validate(any(BindingResult.class), eq(plannedTenderActivityForm)))
        .thenReturn(plannedTenderActivityErrors);

    assertTrue(plannedTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        plannedTenderService,
        hasPlannedTenderFormService,
        plannedTenderActivityService,
        plannedTenderActivityFormService
    );
  }
}