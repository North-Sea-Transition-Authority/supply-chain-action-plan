package uk.co.nstauthority.scap.workarea;

import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.jooq.Condition;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@Service
class WorkAreaFilterService {

  ArrayList<Condition> getConditions(WorkAreaFilter filter) {
    var conditions = new ArrayList<Condition>();

    if (Objects.nonNull(filter.getScapStatuses())) {
      conditions.add(getStatusCondition(filter.getScapStatuses()));
    }

    return conditions;
  }

  private Condition getStatusCondition(List<ScapDetailStatus> statuses) {
    var statusStrings = statuses
        .stream()
        .map(ScapDetailStatus::getEnumName)
        .toList();
    return SCAP_DETAILS.STATUS.in(statusStrings);
  }
}
