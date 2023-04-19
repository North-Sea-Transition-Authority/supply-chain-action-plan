package uk.co.nstauthority.scap.scap.contractingperformance;

import static uk.co.nstauthority.scap.util.TaskListItemUtil.getBindingResultForForm;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.contractingperformance.summary.HasMoreContractingPerformance;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class ContractingPerformanceTaskListItemService {

  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;
  private final ContractingPerformanceService contractingPerformanceService;
  private final ContractingPerformanceFormService contractingPerformanceFormService;

  @Autowired
  ContractingPerformanceTaskListItemService(
      ContractingPerformanceOverviewService contractingPerformanceOverviewService,
      ContractingPerformanceService contractingPerformanceService,
      ContractingPerformanceFormService contractingPerformanceFormService) {
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.contractingPerformanceService = contractingPerformanceService;
    this.contractingPerformanceFormService = contractingPerformanceFormService;
  }

  public boolean isValid(ScapDetail scapDetail) {
    var contractingPerformanceOverviewOpt = contractingPerformanceOverviewService.findByScapDetail(scapDetail);
    if (contractingPerformanceOverviewOpt.isEmpty()) {
      return false;
    }

    var contractingPerformanceOverview = contractingPerformanceOverviewOpt.get();

    if (Boolean.FALSE.equals(contractingPerformanceOverview.getHasContractingPerformance())) {
      return true;
    }

    var contractingPerformances = contractingPerformanceService
        .getAllByContractingPerformanceOverview(contractingPerformanceOverview);

    if (contractingPerformances.isEmpty()) {
      return false;
    }

    var activitiesWithAwardedContracts = contractingPerformances.stream()
        .map(ContractingPerformance::getActualTenderActivity)
        .toList();

    var anyContractingPerformancesInvalid = contractingPerformances.stream()
        .map(contractingPerformanceFormService::getForm)
        .map(form -> contractingPerformanceFormService.validate(
            form, getBindingResultForForm(form), activitiesWithAwardedContracts
        )).anyMatch(BindingResult::hasErrors);

    if (anyContractingPerformancesInvalid) {
      return false;
    }

    return HasMoreContractingPerformance.NO.equals(contractingPerformanceOverview.getHasMoreContractingPerformance());
  }
}
