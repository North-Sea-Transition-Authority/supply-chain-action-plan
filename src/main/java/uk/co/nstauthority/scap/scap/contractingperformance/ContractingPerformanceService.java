package uk.co.nstauthority.scap.scap.contractingperformance;

import java.time.Clock;
import java.util.List;
import java.util.Objects;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;

@Service
public class ContractingPerformanceService {

  private final ContractingPerformanceRepository contractingPerformanceRepository;
  private final ActualTenderActivityService actualTenderActivityService;
  private final Clock clock;

  @Autowired
  ContractingPerformanceService(ContractingPerformanceRepository contractingPerformanceRepository,
                                ActualTenderActivityService actualTenderActivityService, Clock clock) {
    this.contractingPerformanceRepository = contractingPerformanceRepository;
    this.actualTenderActivityService = actualTenderActivityService;
    this.clock = clock;
  }

  public boolean hasContractingPerformance(ContractingPerformanceOverview contractingPerformanceOverview) {
    return contractingPerformanceRepository.existsByContractingPerformanceOverview(contractingPerformanceOverview);
  }

  public boolean hasContractingPerformance(ActualTenderActivity actualTenderActivity) {
    return contractingPerformanceRepository.existsByActualTenderActivity(actualTenderActivity);
  }

  public ContractingPerformance getById(Integer contractingPerformanceId) {
    return contractingPerformanceRepository.findById(contractingPerformanceId)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not find ContractingPerformance with ID [%s]".formatted(contractingPerformanceId)));
  }

  public List<ContractingPerformance> getAllByActualTenderActivities(List<ActualTenderActivity> activities) {
    return contractingPerformanceRepository.getAllByActualTenderActivityIn(activities);
  }

  @Transactional
  void createContractingPerformance(ContractingPerformanceOverview contractingPerformanceOverview,
                                    ContractingPerformanceForm form) {
    var contractingPerformance = new ContractingPerformance(contractingPerformanceOverview, clock.instant());
    saveContractingPerformance(contractingPerformance, form);
  }

  @Transactional
  void saveContractingPerformance(ContractingPerformance contractingPerformance,
                                  ContractingPerformanceForm form) {
    var actualTenderActivity = actualTenderActivityService.getById(form.getActualTenderActivityId());
    contractingPerformance.setActualTenderActivity(actualTenderActivity);
    var outturnCost = form.getOutturnCost()
        .getAsBigDecimal()
        .orElseThrow(() -> new IllegalArgumentException("%s could not be cast to BigDecimal"
            .formatted(form.getOutturnCost().getInputValue())));
    contractingPerformance.setOutturnCost(outturnCost);
    contractingPerformance.setOutturnRationale(form.getOutturnRationale().getInputValue());

    contractingPerformanceRepository.save(contractingPerformance);
  }

  @Transactional
  public void deleteContractingPerformance(ContractingPerformance contractingPerformance) {
    contractingPerformanceRepository.delete(contractingPerformance);
  }

  @Transactional
  public void deleteByActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    contractingPerformanceRepository.deleteByActualTenderActivity(actualTenderActivity);
  }

  public List<ContractingPerformance> getAllByContractingPerformanceOverview(
      ContractingPerformanceOverview contractingPerformanceOverview) {
    return contractingPerformanceRepository.getAllByContractingPerformanceOverview(contractingPerformanceOverview);
  }

  List<ActualTenderActivity> getActivitiesWithoutContractingPerformance(
      List<ActualTenderActivity> contractedActivities) {
    var activityIdsWithContractingPerformance = getActivityIdsWithContractingPerformances(contractedActivities);
    return contractedActivities.stream()
        .filter(actualTenderActivity -> !activityIdsWithContractingPerformance.contains(actualTenderActivity.getId()))
        .toList();
  }

  List<ActualTenderActivity> getActivitiesWithoutContractingPerformancesWithCurrent(
      List<ActualTenderActivity> contractedActivities, ContractingPerformance contractingPerformance) {
    var activityIdsWithContractingPerformance = getActivityIdsWithContractingPerformances(contractedActivities);
    return contractedActivities.stream()
        .filter(actualTenderActivity -> !activityIdsWithContractingPerformance.contains(actualTenderActivity.getId())
            || Objects.equals(actualTenderActivity.getId(), contractingPerformance.getActualTenderActivity().getId()))
        .toList();
  }

  private List<Integer> getActivityIdsWithContractingPerformances(List<ActualTenderActivity> activities) {
    return getAllByActualTenderActivities(activities)
        .stream()
        .map(ContractingPerformance::getActualTenderActivity)
        .map(ActualTenderActivity::getId)
        .toList();
  }
}
