package uk.co.nstauthority.scap.permissionmanagement;

public enum TeamType {

  REGULATOR("Regulator", "Teams for managing regulator users", 10),
  INDUSTRY("Industry", "Teams for managing industry users", 20);

  private final String displayText;
  private final String hintText;
  private final int displayOrder;

  TeamType(String displayText, String hintText, int displayOrder) {
    this.displayText = displayText;
    this.hintText = hintText;
    this.displayOrder = displayOrder;
  }

  public String getDisplayText() {
    return displayText;
  }

  public String getHintText() {
    return hintText;
  }

  public int getDisplayOrder() {
    return displayOrder;
  }

}