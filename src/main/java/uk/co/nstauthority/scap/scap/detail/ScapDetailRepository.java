package uk.co.nstauthority.scap.scap.detail;

import java.util.List;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.scap.Scap;

public interface ScapDetailRepository extends CrudRepository<ScapDetail, Integer> {

  List<ScapDetail> findAllByScap(Scap scap);

  List<ScapDetail> findAllByScapId(Integer scapId);

  List<ScapDetail> findAllByScapIdAndStatus(Integer scapId, ScapDetailStatus status);

  Optional<ScapDetail> findByScapIdAndVersionNumber(Integer scapId, Integer versionNumber);

  Optional<ScapDetail> findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(Integer scapId, List<ScapDetailStatus> status);

  Optional<ScapDetail> findFirstByScapIdAndStatusInOrderByVersionNumberDesc(Integer scapId, List<ScapDetailStatus> status);
}
