package uk.co.nstauthority.scap.application.actualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.time.Instant;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewTaskListSection;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
class ActualTenderTaskListItemTest {

  @Mock
  ScapOverviewService scapOverviewService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  ActualTenderService actualTenderService;

  @InjectMocks
  ActualTenderTaskListItem actualTenderTaskListItem;

  private Integer scapId;
  private ScapOverview scap;
  private ScapDetail scapDetail;

  @BeforeEach
  void setup() {
    scapId = 42;
    scap = new ScapOverview(scapId);
    scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, Instant.now(), 1);
  }

  @Test
  void getItemDisplayText() {
    assertThat(actualTenderTaskListItem.getItemDisplayText()).isEqualTo("Actual tendering activity");
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
    when(scapOverviewService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertFalse(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_existingActualTender_hasNoTenders_assertTrue() {
    var existingActualTender = new ActualTender(scapDetail, Instant.now());
    existingActualTender.setHasActualTenders(false);

    when(scapOverviewService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(existingActualTender));

    assertTrue(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_existingActualTender_allActualTendersAdded_assertTrue() {
    var existingActualTender = new ActualTender(scapDetail, Instant.now());
    existingActualTender.setHasActualTenders(true);
    existingActualTender.setAllActualTendersAdded(true);

    when(scapOverviewService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(existingActualTender));

    assertTrue(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_existingActualTender_notAllActualTendersAdded_assertFalse() {
    var existingActualTender = new ActualTender(scapDetail, Instant.now());
    existingActualTender.setHasActualTenders(true);
    existingActualTender.setAllActualTendersAdded(false);

    when(scapOverviewService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(actualTenderService.getByScapDetail(scapDetail)).thenReturn(Optional.of(existingActualTender));

    assertFalse(actualTenderTaskListItem.isValid(scapId));
  }

  @Test
  void getTaskListSection() {
    assertThat(actualTenderTaskListItem.getTaskListSection()).isEqualTo(ScapOverviewTaskListSection.class);
  }

  @Test
  void isVisible() {
    assertTrue(actualTenderTaskListItem.isVisible(scapId));
  }
}
