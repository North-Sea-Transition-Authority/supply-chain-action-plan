package uk.co.nstauthority.scap.energyportal;

import uk.co.nstauthority.scap.fds.searchselector.SearchSelectable;

class CountrySelectable implements SearchSelectable {

  private final String countryId;
  private final String countryName;

  CountrySelectable(Integer countryId, String countryName) {
    this.countryId = String.valueOf(countryId);
    this.countryName = countryName;
  }

  @Override
  public String getSelectionId() {
    return countryId;
  }

  @Override
  public String getSelectionText() {
    return countryName;
  }
}
