package uk.co.nstauthority.scap.scap.plannedtender;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class PlannedTenderTaskListItemTest {

  @Mock
  private ScapService scapService;

  @Mock
  private ScapDetailService scapDetailService;

  @Mock
  private PlannedTenderService plannedTenderService;

  @Mock
  private PlannedTenderActivityService plannedTenderActivityService;

  @InjectMocks
  private PlannedTenderTaskListItem plannedTenderTaskListItem;

  @Test
  void isValid_noPlannedTenderEntity_assertFalse() {
    var scap = new Scap(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertFalse(plannedTenderTaskListItem.isValid(scap.getId()));
  }

  @Test
  void isValid_hasPlannedTenders_noMoreActivityPlanned_assertTrue() {
    var scap = new Scap(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    var scapPlannedTender = new PlannedTender(scapDetail, Instant.ofEpochSecond(1665566343));
    scapPlannedTender.setHasPlannedTenders(true);
    scapPlannedTender.setHasMorePlannedTenderActivities(HasMorePlannedTenderActivities.NO);

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(scapPlannedTender));

    assertTrue(plannedTenderTaskListItem.isValid(scap.getId()));
  }

  @Test
  void isValid_hasPlannedTenders_nullMoreActivityPlanned_assertFalse() {
    var scap = new Scap(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    var scapPlannedTender = new PlannedTender(scapDetail, Instant.ofEpochSecond(1665566343));
    scapPlannedTender.setHasPlannedTenders(true);

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(scapPlannedTender));

    assertFalse(plannedTenderTaskListItem.isValid(scap.getId()));
  }

  @Test
  void isValid_hasNoPlannedTenders_assertTrue() {
    var scap = new Scap(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    var scapPlannedTender = new PlannedTender(scapDetail, Instant.ofEpochSecond(1665566343));
    scapPlannedTender.setHasPlannedTenders(false);

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(scapPlannedTender));

    assertTrue(plannedTenderTaskListItem.isValid(scap.getId()));
  }

  @Test
  void isValid_hasPlannedTendersIsNull_assertFalse() {
    var scap = new Scap(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    var scapPlannedTender = new PlannedTender(scapDetail, Instant.ofEpochSecond(1665566343));

    when(scapService.getScapById(scap.getId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(scapPlannedTender));

    assertFalse(plannedTenderTaskListItem.isValid(scap.getId()));
  }
}
