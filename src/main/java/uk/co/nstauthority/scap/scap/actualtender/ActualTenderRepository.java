package uk.co.nstauthority.scap.scap.actualtender;

import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

interface ActualTenderRepository extends CrudRepository<ActualTender, Integer> {

  Optional<ActualTender> findByScapDetail(ScapDetail scapDetail);

}
