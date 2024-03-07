package uk.co.nstauthority.scap.scap.plannedtender.list;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.delete.DeletePlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.update.UpdatePlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
class PlannedTenderActivityListServiceTest {

  private PlannedTenderActivityListService plannedTenderActivityListService;

  @BeforeEach
  void setup() {
    plannedTenderActivityListService = new PlannedTenderActivityListService();
  }

  @Test
  void plannedTenderDetailsToListItems() {
    var scapId = new ScapId(17);
    var detail1 = new PlannedTenderActivity(22);
    var detail2 = new PlannedTenderActivity(23);
    var plannedTenderDetails = List.of(detail1, detail2);

    var listItems = plannedTenderActivityListService.plannedTenderDetailsToListItems(scapId, plannedTenderDetails);

    assertThat(listItems).extracting(
        PlannedTenderActivityListItem::detail,
        PlannedTenderActivityListItem::changeLinkUrl,
        PlannedTenderActivityListItem::deleteLinkUrl
    ).containsExactly(
        tuple(detail1,
            ReverseRouter.route(on(UpdatePlannedTenderActivityController.class).renderUpdatePlannedTenderDetail(scapId, detail1.getId())),
            ReverseRouter.route(on(DeletePlannedTenderActivityController.class).renderPlannedTenderRemoval(scapId, detail1.getId()))),
        tuple(detail2,
            ReverseRouter.route(on(UpdatePlannedTenderActivityController.class).renderUpdatePlannedTenderDetail(scapId, detail2.getId())),
            ReverseRouter.route(on(DeletePlannedTenderActivityController.class).renderPlannedTenderRemoval(scapId, detail2.getId())))
    );
  }
}
