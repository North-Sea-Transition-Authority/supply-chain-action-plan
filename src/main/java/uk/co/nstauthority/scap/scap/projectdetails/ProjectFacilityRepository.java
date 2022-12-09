package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.List;
import org.springframework.data.repository.CrudRepository;

interface ProjectFacilityRepository extends CrudRepository<ProjectFacility, Integer> {

  List<ProjectFacility> findAllByProjectDetails(ProjectDetails projectDetails);

}
