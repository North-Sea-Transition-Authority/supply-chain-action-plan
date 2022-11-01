package uk.co.nstauthority.scap.application.projectdetails;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.application.detail.ScapDetail;

interface ProjectDetailsRepository extends CrudRepository<ProjectDetails, Integer> {

  Optional<ProjectDetails> findByScapDetail(ScapDetail scapDetail);
}
