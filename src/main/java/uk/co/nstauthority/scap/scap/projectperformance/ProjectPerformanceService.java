package uk.co.nstauthority.scap.scap.projectperformance;

import java.time.Clock;
import java.util.Optional;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class ProjectPerformanceService {

  private final ProjectPerformanceRepository projectPerformanceRepository;
  private final Clock clock;

  @Autowired
  ProjectPerformanceService(ProjectPerformanceRepository projectPerformanceRepository, Clock clock) {
    this.projectPerformanceRepository = projectPerformanceRepository;
    this.clock = clock;
  }

  public Optional<ProjectPerformance> findByScapDetail(ScapDetail scapDetail) {
    return projectPerformanceRepository.findByScapDetail(scapDetail);
  }

  @Transactional
  void createProjectPerformance(ScapDetail scapDetail, ProjectPerformanceForm form) {
    var projectPerformance = new ProjectPerformance(scapDetail, clock.instant());
    saveProjectPerformance(projectPerformance, form);
  }

  @Transactional
  void saveProjectPerformance(ProjectPerformance projectPerformance, ProjectPerformanceForm form) {
    projectPerformance.setProjectCompleted(YesNo.YES.equals(form.getIsProjectCompleted()));
    if (Boolean.TRUE.equals(projectPerformance.getProjectCompleted())) {
      var startDate = form.getStartDate().getAsLocalDate()
          .orElseThrow(() -> new ClassCastException("Could not get %s/%s/%s as LocalDate".formatted(
              form.getStartDay().getInputValue(),
              form.getStartMonth().getInputValue(),
              form.getStartYear().getInputValue())));
      var completionDate = form.getCompletionDate().getAsLocalDate()
          .orElseThrow(() -> new ClassCastException("Could not get %s/%s/%s as LocalDate".formatted(
              form.getCompletionDay().getInputValue(),
              form.getCompletionMonth().getInputValue(),
              form.getCompletionYear().getInputValue())));
      var outturnCost = form.getOutturnCost().getAsBigDecimal()
              .orElseThrow(() -> new ClassCastException("Could not get %s as BigDecimal".formatted(
                  form.getOutturnCost().getInputValue())));

      projectPerformance.setStartDate(startDate);
      projectPerformance.setCompletionDate(completionDate);
      projectPerformance.setOutturnCost(outturnCost);
    } else {
      projectPerformance.setStartDate(null);
      projectPerformance.setCompletionDate(null);
      projectPerformance.setOutturnCost(null);
    }
    projectPerformanceRepository.save(projectPerformance);
  }
}
