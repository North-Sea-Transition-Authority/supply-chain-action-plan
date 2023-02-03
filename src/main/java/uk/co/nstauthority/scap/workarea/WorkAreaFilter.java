package uk.co.nstauthority.scap.workarea;

import java.io.Serializable;
import java.util.List;
import org.springframework.web.bind.annotation.SessionAttributes;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@SessionAttributes({"workAreaFilter"})
class WorkAreaFilter implements Serializable {

  private List<ScapDetailStatus> scapStatuses;
  private String referenceSearchTerm;

  void update(WorkAreaForm form) {
    scapStatuses = form.getScapStatuses();
    referenceSearchTerm = form.getReferenceSearchTerm();
  }

  void clearFilter() {
    scapStatuses = null;
    referenceSearchTerm = null;
  }

  List<ScapDetailStatus> getScapStatuses() {
    return scapStatuses;
  }

  public String getReferenceSearchTerm() {
    return referenceSearchTerm;
  }
}
