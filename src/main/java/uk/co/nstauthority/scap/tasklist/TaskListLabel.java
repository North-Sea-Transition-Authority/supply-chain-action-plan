package uk.co.nstauthority.scap.tasklist;

public enum TaskListLabel {

  NOT_STARTED,
  IN_PROGRESS,
  COMPLETED,
  NOT_COMPLETED,
  BLOCKED;

  public static TaskListLabel from(boolean isValid) {
    if (isValid) {
      return COMPLETED;
    }
    return NOT_COMPLETED;
  }
}
