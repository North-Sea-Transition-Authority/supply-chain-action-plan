package uk.co.nstauthority.scap.scap.scap;

public record ScapId(Integer scapId) {

  public static ScapId valueOf(String value) {
    return new ScapId(Integer.valueOf(value));
  }

  public static ScapId valueOf(Integer value) {
    return new ScapId(value);
  }

  @Override
  public String toString() {
    return String.valueOf(scapId);
  }
}
