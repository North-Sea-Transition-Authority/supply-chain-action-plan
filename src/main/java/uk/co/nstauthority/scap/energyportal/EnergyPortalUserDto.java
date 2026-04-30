package uk.co.nstauthority.scap.energyportal;

public record EnergyPortalUserDto(
    Long webUserAccountId,
    String title,
    String forename,
    String surname,
    String emailAddress,
    String telephoneNumber,
    boolean canLogin
) {

  public String displayName() {
    return "%s %s".formatted(forename, surname);
  }
}
