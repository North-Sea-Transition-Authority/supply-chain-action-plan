package uk.co.nstauthority.scap.scap.submit;

import org.springframework.stereotype.Component;

@Component
public class ReviewAndSubmitTaskListSection implements uk.co.nstauthority.scap.scap.tasklist.ScapTaskListSection {

  static final String SECTION_NAME = "Review and submit";

  @Override
  public String getSectionName() {
    return SECTION_NAME;
  }

  @Override
  public int getDisplayOrder() {
    return 20;
  }
}
