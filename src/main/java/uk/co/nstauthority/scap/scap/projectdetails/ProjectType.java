package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

public enum ProjectType implements Displayable {
  FIELD_DEVELOPMENT_PLAN("Field development plan", 10),
  FIELD_DEVELOPMENT_PLAN_ADDENDUM("Field development plan addendum", 20),
  DECOMMISSIONING_PROGRAMME("Decommissioning programme", 30),
  CARBON_STORAGE_PERMIT("Carbon storage permit", 40),
  EMISSIONS_REDUCTION_PLAN("Emissions reduction action plan", 50);

  private final String displayName;
  private final Integer displayOrder;

  ProjectType(String displayName, Integer displayOrder) {
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

  public static Map<String, String> getCheckboxItems() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(ProjectType.class);
  }
}
