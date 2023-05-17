package uk.co.nstauthority.scap.scap.copy;

import java.util.List;
import java.util.Set;
import javax.persistence.EntityManager;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderProjectsOverview;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;

@Service
public class EntityCopyService {

  private final EntityManager entityManager;

  @Autowired
  public EntityCopyService(EntityManager entityManager) {
    this.entityManager = entityManager;
  }

  public ScapDetailChild copyChild(ScapDetail newScapDetail, ScapDetailChild child) {

    var newChild = BeanUtils.instantiateClass(child.getClass());
    BeanUtils.copyProperties(child, newChild);

    newChild.setId(null);
    newChild.setScapDetail(newScapDetail);
    entityManager.persist(newChild);
    return newChild;
  }

  public ProjectDetailsChild copyChild(ProjectDetails newProjectDetails, ProjectDetailsChild child) {
    entityManager.detach(child);
    child.setId(null);
    child.setProjectDetails(newProjectDetails);
    entityManager.persist(child);
    return child;
  }

  public PlannedTenderChild copyChild(PlannedTender newPlannedTender, PlannedTenderChild child) {
    entityManager.detach(child);
    child.setId(null);
    child.setPlannedTender(newPlannedTender);
    entityManager.persist(child);
    return child;
  }

  public PathfinderChild copyChild(PathfinderProjectsOverview newPathfinderProjectOverview, PathfinderChild child) {
    entityManager.detach(child);
    child.setId(null);
    child.setPathfinderProjectsOverview(newPathfinderProjectOverview);
    entityManager.persist(child);
    return child;
  }

  public List<ProjectDetailsChild> copyChildren(ProjectDetails newProjectDetails,
                                                          List<? extends ProjectDetailsChild> children) {
    return children.stream()
        .map(child -> copyChild(newProjectDetails, child))
        .toList();
  }

  public List<PlannedTenderChild> copyChildren(PlannedTender newPlannedTender,
                                                         List<? extends PlannedTenderChild> children) {
    return children.stream()
        .map(child -> copyChild(newPlannedTender, child))
        .toList();
  }

  public List<PathfinderChild> copyChildren(PathfinderProjectsOverview newPathfinderProjectOverview,
                                               Set<? extends PathfinderChild> children) {
    return children.stream()
        .map(child -> copyChild(newPathfinderProjectOverview, child))
        .toList();
  }
}
