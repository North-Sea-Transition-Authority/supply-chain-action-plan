package uk.co.nstauthority.scap.energyportal;

import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.countries.CountryApiV2;
import uk.co.fivium.energyportalapi.generated.client.CountriesV2ProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.CountryV2ProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.CountryV2;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class CountryService {

  private final CountryApiV2 countryApi;

  @Autowired
  CountryService(CountryApiV2 countryApi) {
    this.countryApi = countryApi;
  }

  public boolean doesCountryExist(String isoCode) {
    return findCountryByIsoCode(isoCode, "Verify country exists for SCAP").isPresent();
  }

  public Optional<CountryV2> findCountryByIsoCode(String isoCode, String purpose) {
    var filters = new CountryV2ProjectionRoot().isoCode().name();
    var requestPurpose = new RequestPurpose(purpose);

    return countryApi.findCountryByIsoCode(isoCode, filters, requestPurpose);
  }

  public List<CountryV2> getCountriesByIsoCodes(List<String> isoCodes, String purpose) {
    var filters = new CountriesV2ProjectionRoot().isoCode().name();
    var requestPurpose = new RequestPurpose(purpose);

    return countryApi.getAllCountriesByIsoCodesIn(isoCodes, filters, requestPurpose);
  }

  public List<CountryV2> searchCountries(String term, String purpose) {
    var filters = new CountriesV2ProjectionRoot().isoCode().name();
    var requestPurpose = new RequestPurpose(purpose);

    return countryApi.searchActiveCountriesByName(term, filters, requestPurpose);
  }

  public RestSearchResult getCountrySearchResults(List<CountryV2> countries) {
    return new RestSearchResult(
        countries.stream()
            .map(country -> new RestSearchItem(country.getIsoCode(), country.getName()))
            .toList());
  }

}
