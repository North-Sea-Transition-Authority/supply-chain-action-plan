package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.assertj.core.api.Assertions.tuple;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.countries.CountryApiV2;
import uk.co.fivium.energyportalapi.generated.client.CountriesV2ProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.CountryV2ProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.CountryV2;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;

@ExtendWith(MockitoExtension.class)
class CountryServiceTest {

  @Mock
  CountryApiV2 countryApi;

  @InjectMocks
  CountryService countryService;

  @Test
  void searchCountries() {
    var searchTerm = "united kingdom";
    var searchPurpose = "test search purpose";
    var countries = List.of(
        new CountryV2("United Kingdom", "GB"),
        new CountryV2("Continental Shelf United Kingdom Sector", "CSU")
    );
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountriesV2ProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.searchActiveCountriesByName(
        eq(searchTerm),
        any(CountriesV2ProjectionRoot.class),
        any(RequestPurpose.class)))
        .thenReturn(countries);

    var returnedCountries = countryService.searchCountries(searchTerm, searchPurpose);

    verify(countryApi).searchActiveCountriesByName(
        eq(searchTerm),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture());

    assertThat(returnedCountries).isEqualTo(countries);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("isoCode", null),
        entry("name", null)
    );
  }

  @Test
  void findCountryByIsoCode() {
    var searchIsoCode = "GB";
    var searchPurpose = "test search purpose";
    var country = new CountryV2("United Kingdom", searchIsoCode);
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountryV2ProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.findCountryByIsoCode(
        eq(searchIsoCode),
        any(CountryV2ProjectionRoot.class),
        any(RequestPurpose.class)))
        .thenReturn(Optional.of(country));

    var returnedCountry = countryService.findCountryByIsoCode(searchIsoCode, searchPurpose);

    verify(countryApi).findCountryByIsoCode(
        eq(searchIsoCode),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture());

    assertThat(returnedCountry).contains(country);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("isoCode", null),
        entry("name", null)
    );
  }

  @Test
  void doesCountryExist_NotExists_AssertFalse() {
    var searchIsoCode = "GB";
    var searchPurpose = "Verify country exists for SCAP";
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountryV2ProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.findCountryByIsoCode(
        eq(searchIsoCode),
        any(CountryV2ProjectionRoot.class),
        any(RequestPurpose.class)))
        .thenReturn(Optional.empty());

    var doesCountryExist = countryService.doesCountryExist(searchIsoCode);

    verify(countryApi).findCountryByIsoCode(
        eq(searchIsoCode),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture());

    assertFalse(doesCountryExist);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("isoCode", null),
        entry("name", null)
    );
  }

  @Test
  void doesCountryExist_DoesExist_AssertTrue() {
    var searchIsoCode = "GB";
    var searchPurpose = "Verify country exists for SCAP";
    var country = new CountryV2("United Kingdom", searchIsoCode);
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountryV2ProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.findCountryByIsoCode(
        eq(searchIsoCode),
        any(CountryV2ProjectionRoot.class),
        any(RequestPurpose.class)))
        .thenReturn(Optional.of(country));

    var doesCountryExist = countryService.doesCountryExist(searchIsoCode);

    verify(countryApi).findCountryByIsoCode(
        eq(searchIsoCode),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture());

    assertTrue(doesCountryExist);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("isoCode", null),
        entry("name", null)
    );
  }

  @Test
  void findCountriesByIsoCode() {
    var purpose = "test request purpose";
    var countries = List.of(
        new CountryV2("United Kingdom", "GB"),
        new CountryV2("Bahrain", "BHR")
    );
    var countryIsoCodes = List.of(countries.get(0).getIsoCode(), countries.get(1).getIsoCode());
    var requestedFieldsArgumentCaptor = ArgumentCaptor.forClass(CountriesV2ProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.getAllCountriesByIsoCodesIn(
        eq(countryIsoCodes), any(CountriesV2ProjectionRoot.class), any(RequestPurpose.class)))
        .thenReturn(countries);

    var returnedCountries = countryService.getCountriesByIsoCodes(countryIsoCodes, purpose);

    verify(countryApi).getAllCountriesByIsoCodesIn(
        eq(countryIsoCodes),
        requestedFieldsArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture());

    assertThat(returnedCountries).isEqualTo(countries);
    assertThat(requestedFieldsArgumentCaptor.getValue().getFields()).containsExactly(
        entry("isoCode", null),
        entry("name", null)
    );
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(purpose);
  }

  @Test
  void getCountrySearchResults() {
    var countries = List.of(
        new CountryV2("country 2", "2"),
        new CountryV2("country 1", "1")
    );

    var countriesSearchResult = countryService.getCountrySearchResults(countries);

    assertThat(countriesSearchResult.getResults()).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        tuple(countries.get(0).getIsoCode(), countries.get(0).getName()),
        tuple(countries.get(1).getIsoCode(), countries.get(1).getName())
    );
  }
}
