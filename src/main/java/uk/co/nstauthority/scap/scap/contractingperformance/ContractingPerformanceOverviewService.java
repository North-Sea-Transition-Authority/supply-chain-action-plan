package uk.co.nstauthority.scap.scap.contractingperformance;

import java.time.Clock;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class ContractingPerformanceOverviewService {

  private final ContractingPerformanceOverviewRepository contractingPerformanceOverviewRepository;
  private final Clock clock;

  @Autowired
  ContractingPerformanceOverviewService(
      ContractingPerformanceOverviewRepository contractingPerformanceOverviewRepository, Clock clock) {
    this.contractingPerformanceOverviewRepository = contractingPerformanceOverviewRepository;
    this.clock = clock;
  }

  public Optional<ContractingPerformanceOverview> getByScapDetail(ScapDetail scapDetail) {
    return contractingPerformanceOverviewRepository.findByScapDetail(scapDetail);
  }

  @Transactional
  public void saveContractingPerformance(ScapDetail scapDetail, YesNo hasContractingPerformance) {
    var contractingPerformanceOverview = getByScapDetail(scapDetail)
        .orElse(new ContractingPerformanceOverview(scapDetail, clock.instant()));
    contractingPerformanceOverview.setHasContractingPerformance(YesNo.YES.equals(hasContractingPerformance));
    contractingPerformanceOverviewRepository.save(contractingPerformanceOverview);
  }
}
