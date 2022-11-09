package uk.co.nstauthority.scap.scap.plannedtender.list;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.delete.DeletePlannedTenderDetailController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.update.UpdatePlannedTenderDetailController;

@Service
public class PlannedTenderActivityListService {

  public List<PlannedTenderActivityListItem> plannedTenderDetailsToListItems(Integer scapId,
                                                                             List<PlannedTenderActivity> details) {
    return details.stream()
        .map(scapPlannedTenderDetail -> new PlannedTenderActivityListItem(scapPlannedTenderDetail,
            ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
                .renderUpdatePlannedTenderDetail(scapId, scapPlannedTenderDetail.getId())),
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
                .renderPlannedTenderRemoval(scapId, scapPlannedTenderDetail.getId()))))
        .toList();
  }
}
