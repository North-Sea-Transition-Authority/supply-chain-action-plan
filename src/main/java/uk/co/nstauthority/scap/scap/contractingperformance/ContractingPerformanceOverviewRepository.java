package uk.co.nstauthority.scap.scap.contractingperformance;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

interface ContractingPerformanceOverviewRepository extends CrudRepository<ContractingPerformanceOverview, Integer> {

  Optional<ContractingPerformanceOverview> findByScapDetail(ScapDetail scapDetail);
}
