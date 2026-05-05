package uk.co.nstauthority.scap.scap.summary.contractingperformance;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.CountryV2;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Service
public class ContractingPerformanceSummaryViewService {

  public static final String REQUEST_PURPOSE = "Show contracting performance summaries for SCAP";
  private final ContractingPerformanceSummaryDtoRepository contractingPerformanceSummaryDtoRepository;
  private final CountryService countryService;

  @Autowired
  ContractingPerformanceSummaryViewService(
      ContractingPerformanceSummaryDtoRepository contractingPerformanceSummaryDtoRepository,
      CountryService countryService) {
    this.contractingPerformanceSummaryDtoRepository = contractingPerformanceSummaryDtoRepository;
    this.countryService = countryService;
  }

  public Optional<ContractingPerformanceSummaryView> getContractingPerformanceSummaryView(ScapId scapId,
                                                                                          Integer contractingPerformanceId) {
    var contractingPerformanceSummaryDtoOpt = contractingPerformanceSummaryDtoRepository
        .findByScapIdAndContractingPerformanceId(scapId, contractingPerformanceId);
    return contractingPerformanceSummaryDtoOpt.map(contractingPerformanceSummaryDto -> {
      var country = countryService.findCountryByIsoCode(contractingPerformanceSummaryDto.countryIsoCode(), REQUEST_PURPOSE);
      return new ContractingPerformanceSummaryView(
          scapId,
          contractingPerformanceId,
          contractingPerformanceSummaryDto.scopeTitle(),
          contractingPerformanceSummaryDto.scopeDescription(),
          contractingPerformanceSummaryDto.awardValue(),
          contractingPerformanceSummaryDto.remunerationModel(),
          contractingPerformanceSummaryDto.remunerationModelName(),
          contractingPerformanceSummaryDto.contractor(),
          country.map(CountryV2::getName).orElse(null),
          contractingPerformanceSummaryDto.outturnCost(),
          contractingPerformanceSummaryDto.outturnRationale());
    });
  }

  public List<ContractingPerformanceSummaryView> getContractingPerformanceSummaryViews(ScapId scapId) {
    var contractingPerformanceSummaryDtoList = contractingPerformanceSummaryDtoRepository.getAllByScapId(scapId);
    var countryMap = getCountryMap(contractingPerformanceSummaryDtoList);
    return contractingPerformanceSummaryDtoList.stream()
        .map(contractingPerformanceSummaryDto -> new ContractingPerformanceSummaryView(
            scapId,
            contractingPerformanceSummaryDto.contractingPerformanceId(),
            contractingPerformanceSummaryDto.scopeTitle(),
            contractingPerformanceSummaryDto.scopeDescription(),
            contractingPerformanceSummaryDto.awardValue(),
            contractingPerformanceSummaryDto.remunerationModel(),
            contractingPerformanceSummaryDto.remunerationModelName(),
            contractingPerformanceSummaryDto.contractor(),
            countryMap.get(contractingPerformanceSummaryDto.countryIsoCode()),
            contractingPerformanceSummaryDto.outturnCost(),
            contractingPerformanceSummaryDto.outturnRationale()
        )).toList();
  }

  private Map<String, String> getCountryMap(List<ContractingPerformanceSummaryDto> dtoList) {
    var countryIsoCodes = dtoList.stream()
        .map(ContractingPerformanceSummaryDto::countryIsoCode)
        .distinct()
        .toList();
    var countries = countryService.getCountriesByIsoCodes(countryIsoCodes, REQUEST_PURPOSE);
    return countries.stream().collect(Collectors.toMap(
        CountryV2::getIsoCode,
        CountryV2::getName
    ));
  }

}
