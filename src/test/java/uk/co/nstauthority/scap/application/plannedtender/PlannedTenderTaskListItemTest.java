package uk.co.nstauthority.scap.application.plannedtender;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetailService;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
public class PlannedTenderTaskListItemTest {

  @Mock
  private ScapOverviewService scapOverviewService;

  @Mock
  private ScapDetailService scapDetailService;

  @Mock
  private ScapPlannedTenderService scapPlannedTenderService;

  @Mock
  private ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  @InjectMocks
  private PlannedTenderTaskListItem plannedTenderTaskListItem;

  @Test
  public void isValid_noPlannedTenderEntity_assertFalse() {
    var scap = new ScapOverview(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);

    when(scapOverviewService.getScapById(119)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertFalse(plannedTenderTaskListItem.isValid(119));
  }

  @Test
  public void isValid_hasPlannedTendersIsFalse_assertTrue() {
    var scap = new ScapOverview(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    var scapPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    scapPlannedTender.setHasPlannedTenders(false);

    when(scapOverviewService.getScapById(119)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(scapPlannedTender));

    assertTrue(plannedTenderTaskListItem.isValid(119));
  }

  @Test
  public void isValid_hasPlannedTendersIsTrue_noDetailsFound_assertFalse() {
    var scap = new ScapOverview(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    var scapPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    scapPlannedTender.setHasPlannedTenders(true);

    when(scapOverviewService.getScapById(119)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(scapPlannedTender));
    when(scapPlannedTenderDetailService.hasExistingTenderDetails(scapPlannedTender)).thenReturn(false);

    assertFalse(plannedTenderTaskListItem.isValid(119));
  }

  @Test
  public void isValid_hasPlannedTendersIsTrue_detailsFound_assertTrue() {
    var scap = new ScapOverview(119);
    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT,
        EntityTestingUtil.dateToInstant(2000, 4, 23), 1);
    var scapPlannedTender = new ScapPlannedTender(scapDetail, EntityTestingUtil.dateToInstant(2000, 4, 23));
    scapPlannedTender.setHasPlannedTenders(true);

    when(scapOverviewService.getScapById(119)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail))
        .thenReturn(Optional.of(scapPlannedTender));
    when(scapPlannedTenderDetailService.hasExistingTenderDetails(scapPlannedTender)).thenReturn(true);

    assertTrue(plannedTenderTaskListItem.isValid(119));
  }
}
