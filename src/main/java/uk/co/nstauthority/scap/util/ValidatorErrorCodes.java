package uk.co.nstauthority.scap.util;

public enum ValidatorErrorCodes {

  REQUIRED("required"),
  INVALID("invalid"),
  DOES_NOT_EXIST("doesNotExist")
  ;

  private final String errorCode;

  ValidatorErrorCodes(String errorCode) {
    this.errorCode = errorCode;
  }

  public String getErrorCode() {
    return errorCode;
  }
}
