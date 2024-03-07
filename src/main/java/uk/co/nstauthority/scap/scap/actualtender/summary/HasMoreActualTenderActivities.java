package uk.co.nstauthority.scap.scap.actualtender.summary;

import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

public enum HasMoreActualTenderActivities implements Displayable {

  YES_NOW("Yes, I want to add one now", 10),
  YES_LATER("Yes, but I will add it later before I submit my SCAP", 20),
  NO("No, I have added all the actual tender activities I need to", 30);

  private final String displayName;
  private final Integer displayOrder;

  HasMoreActualTenderActivities(String displayName, Integer displayOrder) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
  }

  public static Map<String, String> getRadioItems() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(HasMoreActualTenderActivities.class);
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
