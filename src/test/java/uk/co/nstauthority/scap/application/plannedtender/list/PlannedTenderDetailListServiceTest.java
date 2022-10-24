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
import uk.co.nstauthority.scap.application.plannedtender.detail.update.UpdatePlannedTenderDetailController;
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
    var scapId = 17;
    var detail1 = new ScapPlannedTenderDetail(22);
    var detail2 = new ScapPlannedTenderDetail(23);
    var plannedTenderDetails = List.of(detail1, detail2);

    var listItems = plannedTenderDetailListService.plannedTenderDetailsToListItems(scapId, plannedTenderDetails);

    assertThat(listItems).extracting(
        PlannedTenderDetailListItem::detail,
        PlannedTenderDetailListItem::changeLinkUrl,
        PlannedTenderDetailListItem::deleteLinkUrl
    ).containsExactly(
        tuple(detail1,
            ReverseRouter.route(on(UpdatePlannedTenderDetailController.class).renderUpdatePlannedTenderDetail(scapId, detail1.getId())),
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class).renderPlannedTenderRemoval(scapId, detail1.getId()))),
        tuple(detail2,
            ReverseRouter.route(on(UpdatePlannedTenderDetailController.class).renderUpdatePlannedTenderDetail(scapId, detail2.getId())),
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class).renderPlannedTenderRemoval(scapId, detail2.getId())))
    );
  }
}
