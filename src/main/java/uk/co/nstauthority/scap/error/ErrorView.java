package uk.co.nstauthority.scap.error;

public enum ErrorView {

  DEFAULT_ERROR("scap/error/error"),
  PAGE_NOT_FOUND("scap/error/404"),
  UNAUTHORISED("scap/error/403");

  private final String viewName;

  ErrorView(String viewName) {
    this.viewName = viewName;
  }

  public String getViewName() {
    return viewName;
  }
}
