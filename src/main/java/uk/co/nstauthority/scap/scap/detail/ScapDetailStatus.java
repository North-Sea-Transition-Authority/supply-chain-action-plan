package uk.co.nstauthority.scap.scap.detail;

import java.util.List;
import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

public enum ScapDetailStatus implements Displayable {
  DRAFT("Draft", 10),
  SUBMITTED("Submitted", 20),
  APPROVED("Approved", 30),
  CLOSED_OUT("Project Completed", 40),
  WITHDRAWN("Withdrawn", 50),
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

  public static Map<String, String> getRadioOptions() {
    var radioOptions = DisplayableEnumOptionUtil.getDisplayableOptions(ScapDetailStatus.class);
    radioOptions.remove(DELETED.getEnumName());
    return radioOptions;
  }

  public static List<ScapDetailStatus> getDefaultStatuses() {
    return List.of(DRAFT, SUBMITTED, APPROVED);
  }

  public static List<ScapDetailStatus> getReinstateableStatuses() {
    return List.of(DRAFT, SUBMITTED, APPROVED, WITHDRAWN, CLOSED_OUT);
  }
}
