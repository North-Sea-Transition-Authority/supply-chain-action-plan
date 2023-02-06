package uk.co.nstauthority.scap.workarea;

import java.io.Serializable;
import java.util.List;
import org.springframework.web.bind.annotation.SessionAttributes;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;

@SessionAttributes({"workAreaFilter"})
class WorkAreaFilter implements Serializable {

  private List<ScapDetailStatus> scapStatuses;
  private String referenceSearchTerm;
  private Integer operatorId;
  private Integer fieldId;
  private List<ProjectType> projectTypes;

  void update(WorkAreaForm form) {
    scapStatuses = form.getScapStatuses();
    referenceSearchTerm = form.getReferenceSearchTerm();
    operatorId = form.getOperatorId();
    fieldId = form.getFieldId();
    projectTypes = form.getProjectTypes();
  }

  void clearFilter() {
    scapStatuses = null;
    referenceSearchTerm = null;
    operatorId = null;
    fieldId = null;
    projectTypes = null;
  }

  List<ScapDetailStatus> getScapStatuses() {
    return scapStatuses;
  }

  public String getReferenceSearchTerm() {
    return referenceSearchTerm;
  }

  public Integer getOperatorId() {
    return operatorId;
  }

  Integer getFieldId() {
    return fieldId;
  }

  List<ProjectType> getProjectTypes() {
    return projectTypes;
  }
}
