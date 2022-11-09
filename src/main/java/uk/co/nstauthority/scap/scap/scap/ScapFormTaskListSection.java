package uk.co.nstauthority.scap.scap.scap;

import org.springframework.stereotype.Component;

@Component
public class ScapFormTaskListSection implements uk.co.nstauthority.scap.scap.tasklist.ScapTaskListSection {

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
