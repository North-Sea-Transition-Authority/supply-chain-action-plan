package uk.co.nstauthority.scap.scap.plannedtender.list;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.stereotype.Service;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
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
        .map(plannedTenderActivity -> new PlannedTenderActivityListItem(
            plannedTenderActivity,
            DateUtils.format(plannedTenderActivity.getExpectedActualTenderStartDate()),
            DateUtils.format(plannedTenderActivity.getExpectedContractAwardDate()),
            ReverseRouter.route(on(UpdatePlannedTenderActivityController.class)
                .renderUpdatePlannedTenderDetail(scapId, plannedTenderActivity.getId())),
            ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
                .renderPlannedTenderRemoval(scapId, plannedTenderActivity.getId()))
        ))
        .toList();
  }
}
