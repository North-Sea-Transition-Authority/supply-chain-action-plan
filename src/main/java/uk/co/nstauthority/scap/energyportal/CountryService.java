package uk.co.nstauthority.scap.energyportal;

import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.countries.CountryApi;
import uk.co.fivium.energyportalapi.generated.client.CountriesProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.CountryProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class CountryService {

  private final CountryApi countryApi;

  @Autowired
  CountryService(CountryApi countryApi) {
    this.countryApi = countryApi;
  }

  public boolean doesCountryExist(Integer id) {
    return findCountryById(id, "Verify country exists for SCAP").isPresent();
  }

  public Optional<Country> findCountryById(Integer id, String purpose) {
    var filters = new CountryProjectionRoot().countryId().countryName();
    var requestPurpose = new RequestPurpose(purpose);

    return countryApi.findCountryById(id, filters, requestPurpose);
  }

  public List<Country> getCountriesByIds(List<Integer> countryIds, String purpose) {
    var filters = new CountriesProjectionRoot().countryId().countryName();
    var requestPurpose = new RequestPurpose(purpose);

    return countryApi.getAllCountriesByIds(countryIds, filters, requestPurpose);
  }

  public List<Country> searchCountries(String term, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);
    var filters = new CountriesProjectionRoot().countryId().countryName();

    return countryApi.searchDefaultActiveCountriesByName(
        term,
        filters,
        requestPurpose);
  }

  public RestSearchResult getCountrySearchResults(List<Country> countries) {
    return new RestSearchResult(
        countries.stream()
            .map(country -> new RestSearchItem(String.valueOf(country.getCountryId()), country.getCountryName()))
            .toList());
  }

}
