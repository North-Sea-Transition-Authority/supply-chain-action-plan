package uk.co.nstauthority.scap.scap.projectperformance;

import jakarta.transaction.Transactional;
import java.time.Clock;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
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
  public void createProjectPerformance(ScapDetail scapDetail, ProjectPerformanceForm form) {
    var projectPerformance = new ProjectPerformance(scapDetail, clock.instant());
    updateProjectPerformance(projectPerformance, form);
  }

  @Transactional
  public void saveProjectPerformance(ProjectPerformance projectPerformance) {
    projectPerformanceRepository.save(projectPerformance);
  }

  @Transactional
  public void updateProjectPerformance(ProjectPerformance projectPerformance, ProjectPerformanceForm form) {
    projectPerformance.setProjectCompleted(Boolean.TRUE.equals(form.getProjectCompleted()));
    if (Boolean.TRUE.equals(projectPerformance.getProjectCompleted())) {
      var startDate = form.getStartDate().getAsLocalDate()
          .orElseThrow(() -> new ScapEntityNotFoundException("Could not get start date"));
      var completionDate = form.getCompletionDate().getAsLocalDate()
          .orElseThrow(() -> new ScapEntityNotFoundException("Could not get completed date"));
      var outturnCost = form.getOutturnCost().getAsBigDecimal()
              .orElseThrow(() -> new ClassCastException("Could not get %s as BigDecimal".formatted(
                  form.getOutturnCost().getInputValue())));

      projectPerformance.setStartDate(startDate);
      projectPerformance.setCompletionDate(completionDate);
      projectPerformance.setOutturnCost(outturnCost);
    }
    projectPerformanceRepository.save(projectPerformance);
  }
}
