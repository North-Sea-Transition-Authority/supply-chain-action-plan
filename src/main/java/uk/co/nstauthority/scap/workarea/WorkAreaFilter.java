package uk.co.nstauthority.scap.workarea;

import java.io.Serializable;
import java.util.List;
import org.springframework.web.bind.annotation.SessionAttributes;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@SessionAttributes({"workAreaFilter"})
class WorkAreaFilter implements Serializable {

  private List<ScapDetailStatus> scapStatuses;

  void update(WorkAreaForm form) {
    scapStatuses = form.getScapStatuses();
  }

  void clearFilter() {
    scapStatuses = null;
  }

  List<ScapDetailStatus> getScapStatuses() {
    return scapStatuses;
  }

}
