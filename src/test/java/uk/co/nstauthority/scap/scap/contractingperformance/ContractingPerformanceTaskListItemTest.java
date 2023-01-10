package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceTaskListItemTest {

  @Mock
  ScapService scapService;

  @Mock
  ScapDetailService scapDetailService;

  @Mock
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @InjectMocks
  ContractingPerformanceTaskListItem contractingPerformanceTaskListItem;

  private static final ScapId SCAP_ID = new ScapId(49);

  @Test
  void getItemDisplayText() {
    assertThat(contractingPerformanceTaskListItem.getItemDisplayText())
        .isEqualTo(ContractingPerformanceTaskListItem.DISPLAY_TEXT);
  }

  @Test
  void getActionUrl() {
    var expectedActionUrl = ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(SCAP_ID));

    assertThat(contractingPerformanceTaskListItem.getActionUrl(SCAP_ID.scapId())).isEqualTo(expectedActionUrl);
  }

  @Test
  void getDisplayOrder() {
    assertThat(contractingPerformanceTaskListItem.getDisplayOrder()).isEqualTo(50);
  }

  @Test
  void isValid_NoContractingPerformanceOverview() {
    var scap = new Scap(SCAP_ID);
    var scapDetail = new ScapDetail();
    var contractingPerformanceOverview = new ContractingPerformanceOverview();

    when(scapService.getScapById(SCAP_ID.scapId())).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail)).thenReturn(Optional.empty());

    assertFalse(contractingPerformanceTaskListItem.isValid(SCAP_ID.scapId()));
  }

  @Test
  void isValid_NullHasContractingPerformance() {
    var scapId = 51;
    var scap = new Scap(scapId);
    var scapDetail = new ScapDetail();
    var contractingPerformanceOverview = new ContractingPerformanceOverview();

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(Optional.of(contractingPerformanceOverview));

    assertFalse(contractingPerformanceTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_HasContractingPerformance() {
    var scapId = 51;
    var scap = new Scap(scapId);
    var scapDetail = new ScapDetail();
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(true);

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(Optional.of(contractingPerformanceOverview));

    assertFalse(contractingPerformanceTaskListItem.isValid(scapId));
  }

  @Test
  void isValid_HasNoContractingPerformance() {
    var scapId = 51;
    var scap = new Scap(scapId);
    var scapDetail = new ScapDetail();
    var contractingPerformanceOverview = new ContractingPerformanceOverview();
    contractingPerformanceOverview.setHasContractingPerformance(false);

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(contractingPerformanceOverviewService.getByScapDetail(scapDetail))
        .thenReturn(Optional.of(contractingPerformanceOverview));

    assertTrue(contractingPerformanceTaskListItem.isValid(scapId));
  }

  @Test
  void isVisible() {
    assertTrue(contractingPerformanceTaskListItem.isVisible(0));
  }

  @Test
  void getTaskListSection() {
    assertThat(contractingPerformanceTaskListItem.getTaskListSection()).isEqualTo(ScapFormTaskListSection.class);
  }
}
