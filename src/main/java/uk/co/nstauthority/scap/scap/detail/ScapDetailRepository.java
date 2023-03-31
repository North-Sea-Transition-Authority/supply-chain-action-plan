package uk.co.nstauthority.scap.scap.detail;

import java.util.List;
import java.util.Optional;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.scap.scap.Scap;

public interface ScapDetailRepository extends CrudRepository<ScapDetail, Integer> {

  List<ScapDetail> findAllByScap(Scap scap);

  List<ScapDetail> findAllByScapIdAndStatus(Integer scapId, ScapDetailStatus status);

  Optional<ScapDetail> findFirstByScapIdAndTipFlag(Integer scapId, boolean tipFlag);

  Optional<ScapDetail> findByScapIdAndVersionNumber(Integer scapId, Integer versionNumber);

  Optional<ScapDetail> findFirstByScapIdAndStatusOrderByVersionNumberDesc(Integer scapId, ScapDetailStatus status);

  Optional<ScapDetail> findFirstByScapIdAndStatusNotInOrderByVersionNumberDesc(Integer scapId, List<ScapDetailStatus> status);

  Optional<ScapDetail> findFirstByScapIdAndStatusInOrderByVersionNumberDesc(Integer scapId, List<ScapDetailStatus> status);

  Optional<ScapDetail> findFirstByScapIdAndTipFlagAndStatus(Integer scapId, boolean tipFlag, ScapDetailStatus status);
}
