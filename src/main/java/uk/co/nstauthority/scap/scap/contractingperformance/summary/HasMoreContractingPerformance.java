package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

public enum HasMoreContractingPerformance implements Displayable {

  YES_NOW("Yes, I want to add one now", 10),
  YES_LATER("Yes, but I will add it later before I submit my SCAP", 20),
  NO("No, I have added all the contracting performance I need to", 30);

  private final String displayName;
  private final Integer displayOrder;

  HasMoreContractingPerformance(String displayName, Integer displayOrder) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
  }

  public static Map<String, String> getRadioItems() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(HasMoreContractingPerformance.class);
  }

  @Override
  public String getDisplayName() {
    return displayName;
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
