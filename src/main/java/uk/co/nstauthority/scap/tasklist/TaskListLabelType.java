package uk.co.nstauthority.scap.tasklist;

public enum TaskListLabelType {

  GREY("govuk-tag--grey");

  private final String cssClassName;

  TaskListLabelType(String cssClassName) {
    this.cssClassName = cssClassName;
  }

  public String getCssClassName() {
    return cssClassName;
  }
}
