package uk.co.nstauthority.scap.application.plannedtender.list;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.delete.DeletePlannedTenderDetailController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
class PlannedTenderDetailListServiceTest {

  private PlannedTenderDetailListService plannedTenderDetailListService;

  @BeforeEach
  void setup() {
    plannedTenderDetailListService = new PlannedTenderDetailListService();
  }

  @Test
  void plannedTenderDetailsToListItems() {
    var detail1 = new ScapPlannedTenderDetail(22);
    var detail2 = new ScapPlannedTenderDetail(23);
    var plannedTenderDetails = List.of(detail1, detail2);

    var listItems = plannedTenderDetailListService.plannedTenderDetailsToListItems(17, plannedTenderDetails);

    assertThat(listItems).extracting(
        PlannedTenderDetailListItem::detail,
        PlannedTenderDetailListItem::changeLinkUrl,
        PlannedTenderDetailListItem::deleteLinkUrl
    ).containsExactly(
        tuple(detail1, "#",
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class).renderPlannedTenderRemoval(17, 22))),
        tuple(detail2, "#",
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class).renderPlannedTenderRemoval(17, 23)))
    );
  }
}
