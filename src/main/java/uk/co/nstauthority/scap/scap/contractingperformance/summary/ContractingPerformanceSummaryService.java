package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.Country;
import uk.co.nstauthority.scap.energyportal.CountryService;

@Service
class ContractingPerformanceSummaryService {

  public static final String REQUEST_PURPOSE = "Show contracting performance summaries for SCAP";
  private final ContractingPerformanceSummaryViewRepository contractingPerformanceSummaryViewRepository;
  private final CountryService countryService;

  @Autowired
  ContractingPerformanceSummaryService(ContractingPerformanceSummaryViewRepository contractingPerformanceSummaryViewRepository,
                                       CountryService countryService) {
    this.contractingPerformanceSummaryViewRepository = contractingPerformanceSummaryViewRepository;
    this.countryService = countryService;
  }

  public List<ContractingPerformanceSummaryView> getContractingPerformanceSummaryViews(Integer scapId) {
    return contractingPerformanceSummaryViewRepository.getContractingPerformanceSummaryViewsByScapId(scapId);
  }

  public Map<String, String> getCountryMap(List<ContractingPerformanceSummaryView> views) {
    var countryIds = views.stream()
        .map(ContractingPerformanceSummaryView::countryId)
        .distinct()
        .toList();
    var countries = countryService.getCountriesByIds(countryIds, REQUEST_PURPOSE);
    return countries.stream().collect(Collectors.toMap(
        country -> String.valueOf(country.getCountryId()),
        Country::getCountryName
    ));
  }

}
