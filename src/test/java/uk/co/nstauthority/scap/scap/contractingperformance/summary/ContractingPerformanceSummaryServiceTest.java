package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.mockito.Mockito.when;

import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceSummaryServiceTest {

  @Mock
  ContractingPerformanceSummaryViewRepository contractingPerformanceSummaryViewRepository;

  @Mock
  CountryService countryService;

  @InjectMocks
  ContractingPerformanceSummaryService contractingPerformanceSummaryService;

  @Test
  void getContractingPerformanceSummaryViews_VerifyCallsRepository() {
    var scapId = 33;
    var views = List.of(new ContractingPerformanceSummaryView(
        null, null, null, null, null,
        null, null, null, null, null
    ));

    when(contractingPerformanceSummaryViewRepository.getContractingPerformanceSummaryViewsByScapId(scapId))
        .thenReturn(views);

    var returnedViews = contractingPerformanceSummaryService
        .getContractingPerformanceSummaryViews(scapId);

    assertThat(returnedViews).isEqualTo(views);
  }

  @Test
  void getCountryMap() {
    var countryId = 0;
    var views = List.of(
        new ContractingPerformanceSummaryView(
            null, null, null, null,null,
            null, null, countryId, null, null
        )
    );
    var country = new Country(countryId, "United Kingdom", null, null);

    when(countryService.getCountriesByIds(List.of(countryId), ContractingPerformanceSummaryService.REQUEST_PURPOSE))
        .thenReturn(List.of(country));

    var countriesMap = contractingPerformanceSummaryService.getCountryMap(views);

    assertThat(countriesMap).containsExactly(
        entry(String.valueOf(countryId), country.getCountryName())
    );

  }
}
