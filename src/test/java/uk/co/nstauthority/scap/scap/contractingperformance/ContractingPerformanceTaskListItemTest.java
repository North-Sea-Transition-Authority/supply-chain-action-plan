package uk.co.nstauthority.scap.scap.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceTaskListItemTest {

  @InjectMocks
  ContractingPerformanceTaskListItem contractingPerformanceTaskListItem;

  @Test
  void getItemDisplayText() {
    assertThat(contractingPerformanceTaskListItem.getItemDisplayText())
        .isEqualTo(ContractingPerformanceTaskListItem.DISPLAY_TEXT);
  }

  @Test
  void getActionUrl() {
    var scapId = 49;
    var expectedActionUrl = ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(scapId));

    assertThat(contractingPerformanceTaskListItem.getActionUrl(scapId)).isEqualTo(expectedActionUrl);
  }

  @Test
  void getDisplayOrder() {
    assertThat(contractingPerformanceTaskListItem.getDisplayOrder()).isEqualTo(50);
  }

  @Test
  void isValid() {
    assertFalse(contractingPerformanceTaskListItem.isValid(0));
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
