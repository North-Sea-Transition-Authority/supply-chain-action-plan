package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.List;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;

interface ProjectFieldRepository extends CrudRepository<ProjectField, UUID> {

  List<ProjectField> findAllByProjectDetails(ProjectDetails projectDetails);

}
