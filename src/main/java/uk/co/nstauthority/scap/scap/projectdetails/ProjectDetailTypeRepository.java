package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.List;
import java.util.Set;
import org.springframework.data.repository.CrudRepository;

interface ProjectDetailTypeRepository extends CrudRepository<ProjectDetailType, Integer> {

  Set<ProjectDetailType> findAllByProjectDetails(ProjectDetails projectDetails);

  Set<ProjectDetailType> findAllByProjectDetailsAndProjectTypeIn(ProjectDetails projectDetails, List<ProjectType> projectTypes);

  void deleteAllByProjectDetailsAndProjectTypeIn(ProjectDetails projectDetails, List<ProjectType> projectTypes);


}
