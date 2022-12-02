package uk.co.nstauthority.scap.scap.contractingperformance;

import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;

interface ContractingPerformanceRepository extends CrudRepository<ContractingPerformance, Integer> {

  boolean existsByContractingPerformanceOverview(ContractingPerformanceOverview contractingPerformanceOverview);

  boolean existsByActualTenderActivity(ActualTenderActivity actualTenderActivity);

  void deleteByActualTenderActivity(ActualTenderActivity actualTenderActivity);
}
