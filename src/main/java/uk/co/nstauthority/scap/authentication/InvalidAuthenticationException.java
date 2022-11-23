package uk.co.nstauthority.scap.authentication;

class InvalidAuthenticationException extends RuntimeException {

  public InvalidAuthenticationException(String message) {
    super(message);
  }
}
