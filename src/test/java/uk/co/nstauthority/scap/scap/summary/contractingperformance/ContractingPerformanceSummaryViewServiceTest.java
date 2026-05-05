package uk.co.nstauthority.scap.scap.summary.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.tuple;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.CountryV2;
import uk.co.nstauthority.scap.energyportal.CountryService;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
class ContractingPerformanceSummaryViewServiceTest {

  @Mock
  ContractingPerformanceSummaryDtoRepository contractingPerformanceSummaryDtoRepository;

  @Mock
  CountryService countryService;

  @InjectMocks
  ContractingPerformanceSummaryViewService contractingPerformanceSummaryViewService;

  private ScapId scapId;
  private Integer contractingPerformanceId;
  private CountryV2 country;
  private ContractingPerformanceSummaryDto contractingPerformanceSummaryDto;

  @BeforeEach
  void setup() {
    scapId = new ScapId(11);
    contractingPerformanceId = 123;
    country = new CountryV2("United Kingdom", "GB");
    contractingPerformanceSummaryDto = new ContractingPerformanceSummaryDto(
        contractingPerformanceId, "Scope title", "Scope description", BigDecimal.ONE,
        RemunerationModel.OTHER, "Some remuneration model", "Some contractor",
        country.getIsoCode(), BigDecimal.TEN, "Some outturn rationale");
  }

  @Test
  void getContractingPerformanceSummaryView() {
    when(countryService.findCountryByIsoCode(country.getIsoCode(), ContractingPerformanceSummaryViewService.REQUEST_PURPOSE))
        .thenReturn(Optional.of(country));
    when(contractingPerformanceSummaryDtoRepository.findByScapIdAndContractingPerformanceId(scapId, contractingPerformanceId))
        .thenReturn(Optional.of(contractingPerformanceSummaryDto));

    var view =
        contractingPerformanceSummaryViewService.getContractingPerformanceSummaryView(scapId, contractingPerformanceId);

    assertThat(view).isNotEmpty();
    assertThat(view.get()).extracting(
        ContractingPerformanceSummaryView::scapId,
        ContractingPerformanceSummaryView::contractingPerformanceId,
        ContractingPerformanceSummaryView::scopeTitle,
        ContractingPerformanceSummaryView::scopeDescription,
        ContractingPerformanceSummaryView::awardValue,
        ContractingPerformanceSummaryView::remunerationModel,
        ContractingPerformanceSummaryView::remunerationModelName,
        ContractingPerformanceSummaryView::contractor,
        ContractingPerformanceSummaryView::location,
        ContractingPerformanceSummaryView::outturnCost,
        ContractingPerformanceSummaryView::outturnCostRationale
    ).containsExactly(
        scapId,
        contractingPerformanceSummaryDto.contractingPerformanceId(),
        contractingPerformanceSummaryDto.scopeTitle(),
        contractingPerformanceSummaryDto.scopeDescription(),
        contractingPerformanceSummaryDto.awardValue(),
        contractingPerformanceSummaryDto.remunerationModel(),
        contractingPerformanceSummaryDto.remunerationModelName(),
        contractingPerformanceSummaryDto.contractor(),
        country.getName(),
        contractingPerformanceSummaryDto.outturnCost(),
        contractingPerformanceSummaryDto.outturnRationale()
    );
  }

  @Test
  void getContractingPerformanceSummaryView_WhenNotFound_AssertEmpty() {
    when(contractingPerformanceSummaryDtoRepository.findByScapIdAndContractingPerformanceId(scapId, contractingPerformanceId))
        .thenReturn(Optional.empty());

    var view =
        contractingPerformanceSummaryViewService.getContractingPerformanceSummaryView(scapId, contractingPerformanceId);

    assertThat(view).isEmpty();
  }

  @Test
  void getContractingPerformanceSummaryViews() {
    when(countryService.getCountriesByIsoCodes(List.of(country.getIsoCode()), ContractingPerformanceSummaryViewService.REQUEST_PURPOSE))
        .thenReturn(List.of(country));
    when(contractingPerformanceSummaryDtoRepository.getAllByScapId(scapId))
        .thenReturn(List.of(contractingPerformanceSummaryDto));

    var views = contractingPerformanceSummaryViewService
        .getContractingPerformanceSummaryViews(scapId);

    assertThat(views).extracting(
        ContractingPerformanceSummaryView::scapId,
        ContractingPerformanceSummaryView::contractingPerformanceId,
        ContractingPerformanceSummaryView::scopeTitle,
        ContractingPerformanceSummaryView::scopeDescription,
        ContractingPerformanceSummaryView::awardValue,
        ContractingPerformanceSummaryView::remunerationModel,
        ContractingPerformanceSummaryView::remunerationModelName,
        ContractingPerformanceSummaryView::contractor,
        ContractingPerformanceSummaryView::location,
        ContractingPerformanceSummaryView::outturnCost,
        ContractingPerformanceSummaryView::outturnCostRationale
    ).containsExactly(
        tuple(
            scapId,
            contractingPerformanceSummaryDto.contractingPerformanceId(),
            contractingPerformanceSummaryDto.scopeTitle(),
            contractingPerformanceSummaryDto.scopeDescription(),
            contractingPerformanceSummaryDto.awardValue(),
            contractingPerformanceSummaryDto.remunerationModel(),
            contractingPerformanceSummaryDto.remunerationModelName(),
            contractingPerformanceSummaryDto.contractor(),
            country.getName(),
            contractingPerformanceSummaryDto.outturnCost(),
            contractingPerformanceSummaryDto.outturnRationale()
        )
    );
  }
}
