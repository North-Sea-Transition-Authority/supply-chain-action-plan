package uk.co.nstauthority.scap.application.plannedtender.list;

import java.util.List;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;

@Service
public class PlannedTenderDetailListService {

  public List<PlannedTenderDetailListItem> plannedTenderDetailsToListItems(List<ScapPlannedTenderDetail> details) {
    return details.stream()
        .map(scapPlannedTenderDetail -> new PlannedTenderDetailListItem(scapPlannedTenderDetail,
            // TODO SCAP2022-39: replace with link to change existing planned tender activity
            "#",
            // TODO SCAP2022-38: replace with link to delete planned tender activity
            "#"))
        .toList();
  }
}
