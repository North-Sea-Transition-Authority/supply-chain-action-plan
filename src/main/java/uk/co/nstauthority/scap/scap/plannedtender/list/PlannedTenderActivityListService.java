package uk.co.nstauthority.scap.scap.plannedtender.list;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.delete.DeletePlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.update.UpdatePlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Service
public class PlannedTenderActivityListService {

  public List<PlannedTenderActivityListItem> plannedTenderDetailsToListItems(ScapId scapId,
                                                                             List<PlannedTenderActivity> details) {
    return details.stream()
        .map(scapPlannedTenderDetail -> new PlannedTenderActivityListItem(scapPlannedTenderDetail,
            ReverseRouter.route(on(UpdatePlannedTenderActivityController.class)
                .renderUpdatePlannedTenderDetail(scapId, scapPlannedTenderDetail.getId())),
            ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
                .renderPlannedTenderRemoval(scapId, scapPlannedTenderDetail.getId()))))
        .toList();
  }
}
