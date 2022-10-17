package uk.co.nstauthority.scap.application.plannedtender;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.application.detail.ScapDetail;

interface ScapPlannedTenderRepository extends CrudRepository<ScapPlannedTender, Integer> {

  Optional<ScapPlannedTender> findByScapDetail(ScapDetail scapDetail);
}
