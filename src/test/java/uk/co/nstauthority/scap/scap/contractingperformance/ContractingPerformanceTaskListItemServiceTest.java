package uk.co.nstauthority.scap.scap.contractingperformance;

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
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityBuilder;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.HasMoreContractingPerformance;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceTaskListItemServiceTest {


  @Mock
  private ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @Mock
  private ContractingPerformanceService contractingPerformanceService;

  @Mock
  private ContractingPerformanceFormService contractingPerformanceFormService;

  @InjectMocks
  private ContractingPerformanceTaskListItemService contractingPerformanceTaskListItemService;

  private static final ScapId SCAP_ID = new ScapId(242);
  private static final Scap SCAP = ScapEntityTestUtil.scapBuilder()
      .withScapId(SCAP_ID)
      .build();
  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder()
      .withScap(SCAP)
      .build();

  @Test
  void isValid_NoPlannedTenderOverview_AssertFalse() {
    when(contractingPerformanceOverviewService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.empty());

    assertFalse(contractingPerformanceTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        contractingPerformanceOverviewService,
        contractingPerformanceService,
        contractingPerformanceFormService
    );
  }

  @Test
  void isValid_HasNoPlannedTenderActivities_AssertTrue() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(false);

    when(contractingPerformanceOverviewService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(contractingPerformanceOverview));

    assertTrue(contractingPerformanceTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        contractingPerformanceOverviewService,
        contractingPerformanceService,
        contractingPerformanceFormService
    );
  }

  @Test
  void isValid_HasPlannedTenderActivities_NoneAdded_AssertFalse() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();

    when(contractingPerformanceOverviewService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(contractingPerformanceOverview));
    when(contractingPerformanceService.getAllByContractingPerformanceOverview(contractingPerformanceOverview))
        .thenReturn(Collections.emptyList());

    assertFalse(contractingPerformanceTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        contractingPerformanceOverviewService,
        contractingPerformanceService,
        contractingPerformanceFormService
    );
  }

  @Test
  void isValid_HasInvalidContractingPerformance_AssertFalse() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    var actualTenderActivity = ActualTenderActivityBuilder.newBuilder()
        .build();
    var contractingPerformance = new ContractingPerformance();
    contractingPerformance.setActualTenderActivity(actualTenderActivity);
    var form = new ContractingPerformanceForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithErrors(form);

    when(contractingPerformanceOverviewService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(contractingPerformanceOverview));
    when(contractingPerformanceService.getAllByContractingPerformanceOverview(contractingPerformanceOverview))
        .thenReturn(Collections.singletonList(contractingPerformance));
    when(contractingPerformanceFormService.getForm(contractingPerformance)).thenReturn(form);
    when(contractingPerformanceFormService
        .validate(eq(form), any(BindingResult.class), eq(Collections.singletonList(actualTenderActivity))))
        .thenReturn(bindingResult);

    assertFalse(contractingPerformanceTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        contractingPerformanceOverviewService,
        contractingPerformanceService,
        contractingPerformanceFormService
    );
  }

  @Test
  void isValid_HasMoreContractingPerformanceToAdd_AssertFalse() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasMoreContractingPerformance(HasMoreContractingPerformance.YES_LATER);
    var actualTenderActivity = ActualTenderActivityBuilder.newBuilder()
        .build();
    var contractingPerformance = new ContractingPerformance();
    contractingPerformance.setActualTenderActivity(actualTenderActivity);
    var form = new ContractingPerformanceForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);

    when(contractingPerformanceOverviewService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(contractingPerformanceOverview));
    when(contractingPerformanceService.getAllByContractingPerformanceOverview(contractingPerformanceOverview))
        .thenReturn(Collections.singletonList(contractingPerformance));
    when(contractingPerformanceFormService.getForm(contractingPerformance)).thenReturn(form);
    when(contractingPerformanceFormService
        .validate(eq(form), any(BindingResult.class), eq(Collections.singletonList(actualTenderActivity))))
        .thenReturn(bindingResult);

    assertFalse(contractingPerformanceTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        contractingPerformanceOverviewService,
        contractingPerformanceService,
        contractingPerformanceFormService
    );
  }

  @Test
  void isValid_HasNoMoreContractingPerformanceToAdd_AssertTrue() {
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasMoreContractingPerformance(HasMoreContractingPerformance.NO);
    var actualTenderActivity = ActualTenderActivityBuilder.newBuilder()
        .build();
    var contractingPerformance = new ContractingPerformance();
    contractingPerformance.setActualTenderActivity(actualTenderActivity);
    var form = new ContractingPerformanceForm();
    var bindingResult = ValidatorTestingUtil.bindingResultWithoutErrors(form);

    when(contractingPerformanceOverviewService.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(contractingPerformanceOverview));
    when(contractingPerformanceService.getAllByContractingPerformanceOverview(contractingPerformanceOverview))
        .thenReturn(Collections.singletonList(contractingPerformance));
    when(contractingPerformanceFormService.getForm(contractingPerformance)).thenReturn(form);
    when(contractingPerformanceFormService
        .validate(eq(form), any(BindingResult.class), eq(Collections.singletonList(actualTenderActivity))))
        .thenReturn(bindingResult);

    assertTrue(contractingPerformanceTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(
        contractingPerformanceOverviewService,
        contractingPerformanceService,
        contractingPerformanceFormService
    );
  }
}
