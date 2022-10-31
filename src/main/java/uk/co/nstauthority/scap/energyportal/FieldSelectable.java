package uk.co.nstauthority.scap.energyportal;

import uk.co.nstauthority.scap.fds.searchselector.SearchSelectable;

public class FieldSelectable implements SearchSelectable {

  private String fieldId;
  private String fieldName;

  public String getFieldId() {
    return fieldId;
  }

  public void setFieldId(String fieldId) {
    this.fieldId = fieldId;
  }

  public String getFieldName() {
    return fieldName;
  }

  public void setFieldName(String fieldName) {
    this.fieldName = fieldName;
  }

  @Override
  public String getSelectionId() {
    return this.fieldId;
  }

  @Override
  public String getSelectionText() {
    return this.fieldName;
  }
}
