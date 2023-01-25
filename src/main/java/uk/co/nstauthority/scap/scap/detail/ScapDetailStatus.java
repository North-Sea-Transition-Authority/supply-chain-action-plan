package uk.co.nstauthority.scap.scap.detail;

import uk.co.nstauthority.scap.enumutil.Displayable;

public enum ScapDetailStatus implements Displayable {
  SUBMITTED("Submitted", 10),
  DRAFT("Draft", 20),
  DELETED("Deleted", 100);

  private final String displayName;
  private final Integer displayOrder;

  ScapDetailStatus(String displayName, Integer displayOrder) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
  }

  @Override
  public String getDisplayName() {
    return this.displayName;
  }

  @Override
  public int getDisplayOrder() {
    return this.displayOrder;
  }

  @Override
  public String getEnumName() {
    return this.name();
  }
}
