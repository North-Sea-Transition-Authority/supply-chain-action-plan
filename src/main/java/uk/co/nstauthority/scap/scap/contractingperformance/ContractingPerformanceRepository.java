package uk.co.nstauthority.scap.scap.contractingperformance;

import org.springframework.data.repository.CrudRepository;

interface ContractingPerformanceRepository extends CrudRepository<ContractingPerformance, Integer> {

  boolean existsByContractingPerformanceOverview(ContractingPerformanceOverview contractingPerformanceOverview);
}
