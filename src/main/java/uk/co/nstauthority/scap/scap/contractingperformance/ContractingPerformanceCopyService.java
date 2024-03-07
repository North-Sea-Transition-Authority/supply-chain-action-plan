package uk.co.nstauthority.scap.scap.contractingperformance;

import static uk.co.nstauthority.scap.scap.detail.NewScapType.DRAFT_UPDATE;

import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.copy.CopyService;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class ContractingPerformanceCopyService implements CopyService {

  private final ContractingPerformanceOverviewService contractingOverviewService;

  private final ContractingPerformanceOverviewRepository contractingOverviewRepository;

  private final EntityCopyService entityCopyService;

  public ContractingPerformanceCopyService(ContractingPerformanceOverviewService contractingOverviewService,
                                           ContractingPerformanceOverviewRepository contractingOverviewRepository,
                                           EntityCopyService entityCopyService) {
    this.contractingOverviewService = contractingOverviewService;
    this.contractingOverviewRepository = contractingOverviewRepository;
    this.entityCopyService = entityCopyService;
  }

  @Override
  public int runOrder() {
    return 20;
  }

  @Override
  public void copyEntity(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType newScapType) {
    var oldContractingPerofmanceOverview = contractingOverviewService.getByScapDetail(oldScapDetail);
    var newContractingPerfomanceOverview = (ContractingPerformanceOverview) entityCopyService
        .copyChild(newScapDetail, oldContractingPerofmanceOverview);

    if (DRAFT_UPDATE.equals(newScapType)) {
      newContractingPerfomanceOverview.setHasContractingPerformance(null);
      newContractingPerfomanceOverview.setHasMoreContractingPerformance(null);
    }
    contractingOverviewRepository.save(newContractingPerfomanceOverview);
  }
}
