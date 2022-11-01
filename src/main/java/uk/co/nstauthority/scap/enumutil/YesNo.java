package uk.co.nstauthority.scap.enumutil;

import java.util.Map;

public enum YesNo implements Displayable {
  YES("Yes", 10),
  NO("No", 20);

  private final String displayName;
  private final Integer displayOrder;

  YesNo(String displayName, Integer displayOrder) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
  }

  public static Map<String, String> getRadioOptions() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(YesNo.class);
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
