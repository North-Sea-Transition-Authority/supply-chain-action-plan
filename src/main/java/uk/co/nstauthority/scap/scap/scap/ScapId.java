package uk.co.nstauthority.scap.scap.scap;

public record ScapId(Integer scapId) {

  public static ScapId valueOf(String value) {
    return new ScapId(Integer.valueOf(value));
  }

  @Override
  public String toString() {
    return String.valueOf(scapId);
  }
}
