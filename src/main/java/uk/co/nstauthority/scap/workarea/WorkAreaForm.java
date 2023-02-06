package uk.co.nstauthority.scap.workarea;

import java.util.List;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

public class WorkAreaForm {

  private List<ScapDetailStatus> scapStatuses;
  private String referenceSearchTerm;
  private Integer operatorId;
  private Integer fieldId;

  public List<ScapDetailStatus> getScapStatuses() {
    return scapStatuses;
  }

  public void setScapStatuses(List<ScapDetailStatus> scapStatuses) {
    this.scapStatuses = scapStatuses;
  }

  public String getReferenceSearchTerm() {
    return referenceSearchTerm;
  }

  public void setReferenceSearchTerm(String referenceSearchTerm) {
    this.referenceSearchTerm = referenceSearchTerm;
  }

  public Integer getOperatorId() {
    return operatorId;
  }

  public void setOperatorId(Integer operatorId) {
    this.operatorId = operatorId;
  }

  public Integer getFieldId() {
    return fieldId;
  }

  public void setFieldId(Integer fieldId) {
    this.fieldId = fieldId;
  }

  public static WorkAreaForm from(WorkAreaFilter filter) {
    var form = new WorkAreaForm();
    form.setScapStatuses(filter.getScapStatuses());
    form.setReferenceSearchTerm(filter.getReferenceSearchTerm());
    form.setOperatorId(filter.getOperatorId());
    form.setFieldId(filter.getFieldId());
    return form;
  }
}
