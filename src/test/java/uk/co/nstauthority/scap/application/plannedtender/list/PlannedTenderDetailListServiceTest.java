package uk.co.nstauthority.scap.application.plannedtender.list;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;

import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;

@ExtendWith(MockitoExtension.class)
class PlannedTenderDetailListServiceTest {

  private PlannedTenderDetailListService plannedTenderDetailListService;

  @BeforeEach
  void setup() {
    plannedTenderDetailListService = new PlannedTenderDetailListService();
  }

  @Test
  void plannedTenderDetailsToListItems() {
    var detail1 = new ScapPlannedTenderDetail();
    var detail2 = new ScapPlannedTenderDetail();
    var plannedTenderDetails = List.of(detail1, detail2);

    var listItems = plannedTenderDetailListService.plannedTenderDetailsToListItems(plannedTenderDetails);

    assertThat(listItems).extracting(
        PlannedTenderDetailListItem::detail,
        PlannedTenderDetailListItem::changeLinkUrl,
        PlannedTenderDetailListItem::deleteLinkUrl
    ).containsExactly(
        tuple(detail1, "#", "#"),
        tuple(detail2, "#", "#")
    );
  }
}
