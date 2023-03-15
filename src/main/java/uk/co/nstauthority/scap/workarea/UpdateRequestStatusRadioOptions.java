package uk.co.nstauthority.scap.workarea;

import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

public enum UpdateRequestStatusRadioOptions implements Displayable {
  ALL("All", 10),
  UPDATE_REQUESTED("Show only update requested", 20),
  UPDATE_OVERDUE("Show only overdue", 30);

  private final String displayName;

  private final Integer sortOrder;

  UpdateRequestStatusRadioOptions(String displayName, Integer sortOrder) {
    this.displayName = displayName;
    this.sortOrder = sortOrder;
  }

  @Override
  public String getDisplayName() {
    return displayName;
  }

  @Override
  public String getEnumName() {
    return this.name();
  }

  @Override
  public int getDisplayOrder() {
    return sortOrder;
  }

  public static Map<String, String> getRadioOptions() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(UpdateRequestStatusRadioOptions.class);
  }
}
