package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
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
import uk.co.fivium.energyportalapi.client.LogCorrelationId;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.countries.CountryApi;
import uk.co.fivium.energyportalapi.generated.client.CountriesProjectionRoot;
import uk.co.fivium.energyportalapi.generated.client.CountryProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.fivium.energyportalapi.generated.types.PortalCountrySet;
import uk.co.fivium.energyportalapi.generated.types.PortalCountryStatus;

@ExtendWith(MockitoExtension.class)
class CountryServiceTest {

  @Mock
  CountryApi countryApi;

  @InjectMocks
  CountryService countryService;

  @Test
  void searchCountries() {
    var searchTerm = "united kingdom";
    var searchPurpose = "test search purpose";
    var countries = List.of(
        new Country(0, "United Kingdom", null, null),
        new Country(68, "Continental Shelf United Kingdom Sector", null, null)
    );
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountriesProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.searchDefaultActiveCountriesByName(
        eq(searchTerm),
        any(CountriesProjectionRoot.class),
        any(RequestPurpose.class),
        any())
    ).thenReturn(countries);

    var returnedCountries = countryService.searchCountries(searchTerm, searchPurpose);

    verify(countryApi).searchDefaultActiveCountriesByName(
        eq(searchTerm),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture(),
        any());

    assertThat(returnedCountries).isEqualTo(countries);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("countryId", null),
        entry("countryName", null)
    );
  }

  @Test
  void findCountryById() {
    var searchId = 0;
    var searchPurpose = "test search purpose";
    var country = new Country(0, "United Kingdom", null, null);
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountryProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.findCountryById(
        eq(searchId),
        any(CountryProjectionRoot.class),
        any(RequestPurpose.class),
        any())
    ).thenReturn(Optional.of(country));

    var returnedCountry = countryService.findCountryById(searchId, searchPurpose);

    verify(countryApi).findCountryById(
        eq(searchId),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture(),
        any());

    assertThat(returnedCountry).contains(country);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("countryId", null),
        entry("countryName", null)
    );
  }

  @Test
  void doesCountryExist_NotExists_AssertFalse() {
    var searchId = 0;
    var searchPurpose = "Verify country exists for SCAP";
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountryProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.findCountryById(
        eq(searchId),
        any(CountryProjectionRoot.class),
        any(RequestPurpose.class),
        any())
    ).thenReturn(Optional.empty());

    var doesCountryExist = countryService.doesCountryExist(searchId);

    verify(countryApi).findCountryById(
        eq(searchId),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture(),
        any());

    assertFalse(doesCountryExist);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("countryId", null),
        entry("countryName", null)
    );
  }

  @Test
  void doesCountryExist_DoesExist_AssertTrue() {
    var searchId = 0;
    var searchPurpose = "Verify country exists for SCAP";
    var country = new Country(0, "United Kingdom", null, null);
    var countryProjectionRootArgumentCaptor = ArgumentCaptor.forClass(CountryProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.findCountryById(
        eq(searchId),
        any(CountryProjectionRoot.class),
        any(RequestPurpose.class),
        any())
    ).thenReturn(Optional.of(country));

    var doesCountryExist = countryService.doesCountryExist(searchId);

    verify(countryApi).findCountryById(
        eq(searchId),
        countryProjectionRootArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture(),
        any());

    assertTrue(doesCountryExist);
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(searchPurpose);
    assertThat(countryProjectionRootArgumentCaptor.getValue().getFields()).containsExactly(
        entry("countryId", null),
        entry("countryName", null)
    );
  }

  @Test
  void findCountriesByIds() {
    var purpose = "test request purpose";
    var countries = List.of(
        new Country(0, "United Kingdom", PortalCountryStatus.ACTIVE, PortalCountrySet.EXPORT_CONTROL),
        new Country(27, "Bahrain", PortalCountryStatus.ACTIVE, PortalCountrySet.EXPORT_CONTROL)
    );
    var countryIds = List.of(countries.get(0).getCountryId(), countries.get(1).getCountryId());
    var requestedFieldsArgumentCaptor = ArgumentCaptor.forClass(CountriesProjectionRoot.class);
    var requestPurposeArgumentCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    when(countryApi.getAllCountriesByIds(
        eq(countryIds), any(CountriesProjectionRoot.class), any(RequestPurpose.class), any(LogCorrelationId.class)))
        .thenReturn(countries);

    var returnedCountries = countryService.getCountriesByIds(countryIds, purpose);

    verify(countryApi).getAllCountriesByIds(
        eq(countryIds),
        requestedFieldsArgumentCaptor.capture(),
        requestPurposeArgumentCaptor.capture(),
        any(LogCorrelationId.class));

    assertThat(returnedCountries).isEqualTo(countries);
    assertThat(requestedFieldsArgumentCaptor.getValue().getFields()).containsExactly(
        entry("countryId", null),
        entry("countryName", null)
    );
    assertThat(requestPurposeArgumentCaptor.getValue().purpose()).isEqualTo(purpose);
  }
}
