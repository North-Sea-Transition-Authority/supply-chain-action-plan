package uk.co.nstauthority.scap.scap.detail;

import java.util.List;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.scap.Scap;

public interface ScapDetailRepository extends CrudRepository<ScapDetail, Integer> {

  List<ScapDetail> findAllByScap(Scap scap);

  Optional<ScapDetail> findFirstByScapIdAndTipFlag(Integer scapId, boolean tipFlag);
}
