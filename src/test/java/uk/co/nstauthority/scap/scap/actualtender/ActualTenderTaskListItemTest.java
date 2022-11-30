package uk.co.nstauthority.scap.scap.actualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class ActualTenderTaskListItemTest {

  @Mock
  ScapService scapService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  ActualTenderService actualTenderService;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ActualTenderTaskListItem actualTenderTaskListItem;

  private Integer scapId;
  private Scap scap;
  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapId = 42;
    scap = new Scap(scapId);
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, clock.instant(), 1);
  }

  @Test
  void getItemDisplayText() {
    assertThat(actualTenderTaskListItem.getItemDisplayText()).isEqualTo("Actual tender activity");
  }

  @Test
  void getActionUrl() {
    var expectedUrl = ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));

    var returnedUrl = actualTenderTaskListItem.getActionUrl(scapId);

    assertThat(returnedUrl).isEqualTo(expectedUrl);
  }

  @Test
  void getDisplayOrder() {
    assertThat(actualTenderTaskListItem.getDisplayOrder()).isEqualTo(40);
  }

  @Test
  void isValid_noActualTender_assertFalse() {
    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertFalse(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_existingActualTender_hasNoTenders_assertTrue() {
    var existingActualTender = new ActualTender(scapDetail, clock.instant());
    existingActualTender.setHasActualTenders(false);

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(existingActualTender));

    assertTrue(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_existingActualTender_allActualTendersAdded_assertTrue() {
    var existingActualTender = new ActualTender(scapDetail, clock.instant());
    existingActualTender.setHasActualTenders(true);
    existingActualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.NO);

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(existingActualTender));

    assertTrue(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_existingActualTender_notAllActualTendersAdded_assertFalse() {
    var existingActualTender = new ActualTender(scapDetail, clock.instant());
    existingActualTender.setHasActualTenders(true);
    existingActualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.YES_LATER);

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(existingActualTender));

    assertFalse(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void getTaskListSection() {
    assertThat(actualTenderTaskListItem.getTaskListSection()).isEqualTo(ScapFormTaskListSection.class);
  }

  @Test
  void isVisible() {
    assertTrue(actualTenderTaskListItem.isVisible(scapId));
  }
}
