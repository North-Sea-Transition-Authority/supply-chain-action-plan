package uk.co.nstauthority.scap.energyportal;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.LogCorrelationId;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.countries.CountryApi;
import uk.co.fivium.energyportalapi.generated.client.CountriesProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.CountryProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Country;

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

    return countryApi.findCountryById(id, filters, requestPurpose, getLogCorrelationId());
  }

  public List<Country> searchCountries(String term, String purpose) {
    var requestPurpose = new RequestPurpose(purpose);
    var filters = new CountriesProjectionRoot().countryId().countryName();

    return countryApi.searchDefaultActiveCountriesByName(
        term,
        filters,
        requestPurpose,
        getLogCorrelationId());
  }

  public static List<CountrySelectable> getCountrySearchableResults(List<Country> countries) {
    return countries.stream()
        .map(country -> new CountrySelectable(country.getCountryId(), country.getCountryName()))
        .toList();
  }

  private LogCorrelationId getLogCorrelationId() {
    return new LogCorrelationId(String.valueOf(UUID.randomUUID()));
  }
}
