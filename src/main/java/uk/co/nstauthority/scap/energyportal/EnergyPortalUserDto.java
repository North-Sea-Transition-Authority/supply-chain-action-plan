package uk.co.nstauthority.scap.energyportal;

public record EnergyPortalUserDto(
    long webUserAccountId,
    String title,
    String forename,
    String surname,
    String emailAddress,
    String telephoneNumber,
    boolean isSharedAccount,
    boolean canLogin
) {

  public String displayName() {
    return "%s %s".formatted(forename, surname);
  }
}