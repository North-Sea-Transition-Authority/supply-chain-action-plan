package uk.co.nstauthority.scap.scap.projectperformance;

import javax.persistence.EntityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.copy.CopyService;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
public class ProjectPerformanceCopyService implements CopyService {

  private final ProjectPerformanceService projectPerformanceService;

  private final EntityCopyService entityCopyService;

  private final EntityManager entityManager;

  @Autowired
  public ProjectPerformanceCopyService(ProjectPerformanceService projectPerformanceService,
                                       EntityCopyService entityCopyService, EntityManager entityManager) {
    this.projectPerformanceService = projectPerformanceService;
    this.entityCopyService = entityCopyService;
    this.entityManager = entityManager;
  }

  @Override
  public int runOrder() {
    return 50;
  }

  @Override
  public void copyEntity(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType newScapType) {
    var oldProjectPerformance = projectPerformanceService.findByScapDetail(oldScapDetail)
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "No Project Performance for SCAP ID: %S".formatted(oldScapDetail.getScap().getId())));
    var newProjectPerformance = (ProjectPerformance) entityCopyService.copyChild(newScapDetail, oldProjectPerformance);
    if (newScapType.equals(NewScapType.DRAFT_UPDATE) && !newProjectPerformance.getProjectCompleted()) {
      newProjectPerformance.setProjectCompleted(null);
      entityManager.persist(newProjectPerformance);
    }
  }
}
