package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.Set;
import org.springframework.data.repository.CrudRepository;

interface ProjectDetailTypeRepository extends CrudRepository<ProjectDetailType, Integer> {

  Set<ProjectDetailType> findAllByProjectDetails(ProjectDetails projectDetails);

}
