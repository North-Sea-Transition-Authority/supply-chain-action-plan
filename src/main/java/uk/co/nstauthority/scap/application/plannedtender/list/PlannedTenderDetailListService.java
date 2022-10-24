package uk.co.nstauthority.scap.application.plannedtender.list;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;
import uk.co.nstauthority.scap.application.plannedtender.detail.delete.DeletePlannedTenderDetailController;
import uk.co.nstauthority.scap.application.plannedtender.detail.update.UpdatePlannedTenderDetailController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Service
public class PlannedTenderDetailListService {

  public List<PlannedTenderDetailListItem> plannedTenderDetailsToListItems(Integer scapId,
                                                                           List<ScapPlannedTenderDetail> details) {
    return details.stream()
        .map(scapPlannedTenderDetail -> new PlannedTenderDetailListItem(scapPlannedTenderDetail,
            ReverseRouter.route(on(UpdatePlannedTenderDetailController.class)
                .renderUpdatePlannedTenderDetail(scapId, scapPlannedTenderDetail.getId())),
            ReverseRouter.route(on(DeletePlannedTenderDetailController.class)
                .renderPlannedTenderRemoval(scapId, scapPlannedTenderDetail.getId()))))
        .toList();
  }
}
