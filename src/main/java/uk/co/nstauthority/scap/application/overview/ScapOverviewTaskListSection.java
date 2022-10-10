package uk.co.nstauthority.scap.application.overview;

import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.application.tasklist.ScapTaskListSection;

@Component
public class ScapOverviewTaskListSection implements ScapTaskListSection {

  public static final String SECTION_NAME = "Overview";

  @Override
  public String getSectionName() {
    return SECTION_NAME;
  }

  @Override
  public int getDisplayOrder() {
    return 10;
  }
}
