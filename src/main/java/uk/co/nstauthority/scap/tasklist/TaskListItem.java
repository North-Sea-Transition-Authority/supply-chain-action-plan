package uk.co.nstauthority.scap.tasklist;

public record TaskListItem(
    String displayName,
    TaskListLabel label,
    String labelHint,
    String actionUrl
) {

  public static TaskListItem withoutLabel(String displayName, String actionUrl) {
    return new TaskListItem(displayName, null, null, actionUrl);
  }

  public TaskListItem(String displayName, TaskListLabel label, String actionUrl) {
    this(displayName, label, null, actionUrl);
  }

}
