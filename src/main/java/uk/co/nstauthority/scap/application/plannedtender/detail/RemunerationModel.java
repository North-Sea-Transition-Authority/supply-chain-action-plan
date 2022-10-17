package uk.co.nstauthority.scap.application.plannedtender.detail;

import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

public enum RemunerationModel implements Displayable {
  LUMP_SUM("Lump sum", 10),
  TARGET_COST("Target cost", 20),
  REIMBURSABLE("Reimbursable", 30),
  OTHER("Other", 40);

  private final String displayName;
  private final int displayOrder;

  RemunerationModel(String displayName, int displayOrder) {
    this.displayName = displayName;
    this.displayOrder = displayOrder;
  }

  public static Map<String, String> getRemunerationModels() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(RemunerationModel.class);
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
