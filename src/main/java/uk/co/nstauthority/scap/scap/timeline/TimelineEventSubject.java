package uk.co.nstauthority.scap.scap.timeline;

public enum TimelineEventSubject {
  SCAP_SUBMITTED("Submitted SCAP");

  private String displayName;

  TimelineEventSubject(String eventDisplayName) {
    displayName = eventDisplayName;
  }

  public String getDisplayName() {
    return displayName;
  }
}
