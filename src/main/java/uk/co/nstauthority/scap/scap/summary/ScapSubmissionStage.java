package uk.co.nstauthority.scap.scap.summary;

import uk.co.nstauthority.scap.enumutil.Displayable;

enum ScapSubmissionStage implements Displayable {
  PROJECT_COMPLETED("Project completed", 10),
  CONTRACTING_PERFORMANCE("Contracting performance", 20),
  ACTUAL_TENDER("Actual tender", 30),
  PLANNED_TENDER("Planned tender", 40),
  CONTRACTING_STRATEGY_PENDING("Contracting strategy pending", 50),
  DRAFT("Draft", 60);

  private final String displayName;
  private final Integer displayOrder;

  ScapSubmissionStage(String displayName, Integer displayOrder) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
  }

  @Override
  public String getDisplayName() {
    return displayName;
  }

  @Override
  public int getDisplayOrder() {
    return displayOrder;
  }

  @Override
  public String getEnumName() {
    return this.name();
  }
}
