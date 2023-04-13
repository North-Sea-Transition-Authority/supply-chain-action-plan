package uk.co.nstauthority.scap.scap.actualtender;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityBuilder;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryViewBuilder;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;

@ExtendWith(MockitoExtension.class)
class ActualTenderTaskListItemServiceTest {


  @Mock
  private ActualTenderService actualTenderService;

  @Mock
  private ActualTenderActivityService actualTenderActivityService;

  @Mock
  private ActualTenderSummaryViewService actualTenderSummaryViewService;

  @InjectMocks
  private ActualTenderTaskListItemService actualTenderTaskListItemService;

  private static final ScapId SCAP_ID = new ScapId(242);
  private static final Scap SCAP = ScapEntityTestUtil.scapBuilder()
      .withScapId(SCAP_ID)
      .build();
  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder()
      .withScap(SCAP)
      .build();

  @Test
  void isValid_NoActualTenderOverview_AssertFalse() {
    when(actualTenderService.getByScapDetail(SCAP_DETAIL)).thenReturn(Optional.empty());

    assertFalse(actualTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(actualTenderService, actualTenderActivityService, actualTenderSummaryViewService);
  }

  @Test
  void isValid_HasNoActualTendersToAdd_AssertTrue() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(false);

    when(actualTenderService.getByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(actualTender));

    assertTrue(actualTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(actualTenderService, actualTenderActivityService, actualTenderSummaryViewService);
  }

  @Test
  void isValid_HasActualTendersToAdd_NoActualTenderActivities_AssertFalse() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(true);

    when(actualTenderService.getByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(Collections.emptyList());

    assertFalse(actualTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(actualTenderService, actualTenderActivityService, actualTenderSummaryViewService);
  }

  @Test
  void isValid_HasActualTendersToAdd_HasInvalidActivities_AssertFalse() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(true);
    var actualTenderActivity = ActualTenderActivityBuilder.newBuilder()
        .withActualTender(actualTender)
        .build();
    var actualTenderActivities = Collections.singletonList(actualTenderActivity);
    var view1 = ActualTenderActivitySummaryViewBuilder.newBuilder()
        .withValid(false)
        .build();
    var view2 = ActualTenderActivitySummaryViewBuilder.newBuilder()
        .withValid(true)
        .build();

    when(actualTenderService.getByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, SCAP_ID))
        .thenReturn(List.of(view1, view2));

    assertFalse(actualTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(actualTenderService, actualTenderActivityService, actualTenderSummaryViewService);
  }

  @Test
  void isValid_HasMoreActualTendersToAdd_AssertFalse() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(true);
    actualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.YES_LATER);
    var actualTenderActivity = ActualTenderActivityBuilder.newBuilder()
        .withActualTender(actualTender)
        .build();
    var actualTenderActivities = Collections.singletonList(actualTenderActivity);
    var view1 = ActualTenderActivitySummaryViewBuilder.newBuilder()
        .withValid(true)
        .build();
    var view2 = ActualTenderActivitySummaryViewBuilder.newBuilder()
        .withValid(true)
        .build();

    when(actualTenderService.getByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, SCAP_ID))
        .thenReturn(List.of(view1, view2));

    assertFalse(actualTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(actualTenderService, actualTenderActivityService, actualTenderSummaryViewService);
  }

  @Test
  void isValid_HasNoMoreActualTendersToAdd_AssertTrue() {
    var actualTender = new ActualTender();
    actualTender.setHasActualTenders(true);
    actualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.NO);
    var actualTenderActivity = ActualTenderActivityBuilder.newBuilder()
        .withActualTender(actualTender)
        .build();
    var actualTenderActivities = Collections.singletonList(actualTenderActivity);
    var view1 = ActualTenderActivitySummaryViewBuilder.newBuilder()
        .withValid(true)
        .build();
    var view2 = ActualTenderActivitySummaryViewBuilder.newBuilder()
        .withValid(true)
        .build();

    when(actualTenderService.getByScapDetail(SCAP_DETAIL)).thenReturn(Optional.of(actualTender));
    when(actualTenderActivityService.getAllByActualTender(actualTender)).thenReturn(actualTenderActivities);
    when(actualTenderSummaryViewService.getByActualTenderActivities(actualTenderActivities, SCAP_ID))
        .thenReturn(List.of(view1, view2));

    assertTrue(actualTenderTaskListItemService.isValid(SCAP_DETAIL));

    verifyNoMoreInteractions(actualTenderService, actualTenderActivityService, actualTenderSummaryViewService);
  }
}
