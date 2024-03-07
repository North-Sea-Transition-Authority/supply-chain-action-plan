package uk.co.nstauthority.scap.feedback;

import java.util.Map;
import uk.co.nstauthority.scap.enumutil.Displayable;
import uk.co.nstauthority.scap.enumutil.DisplayableEnumOptionUtil;

enum SatisfactionRating implements Displayable {

  VERY_SATISFIED("Very satisfied", 1),
  SATISFIED("Satisfied", 2),
  NEITHER("Neither satisfied or dissatisfied", 3),
  DISSATISFIED("Dissatisfied", 4),
  VERY_DISSATISFIED("Very dissatisfied", 5);

  private final String displayText;
  private final int displayOrder;

  SatisfactionRating(String displayText, int displayOrder) {
    this.displayText = displayText;
    this.displayOrder = displayOrder;
  }

  @Override
  public String getDisplayName() {
    return displayText;
  }

  @Override
  public int getDisplayOrder() {
    return displayOrder;
  }

  @Override
  public String getEnumName() {
    return this.name();
  }

  public static Map<String, String> getRadioItems() {
    return DisplayableEnumOptionUtil.getDisplayableOptions(SatisfactionRating.class);
  }
}
