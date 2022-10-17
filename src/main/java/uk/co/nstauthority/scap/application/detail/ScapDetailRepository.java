package uk.co.nstauthority.scap.application.detail;

import java.util.List;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.application.overview.ScapOverview;

public interface ScapDetailRepository extends CrudRepository<ScapDetail, Integer> {

  List<ScapDetail> findAllByScap(ScapOverview scap);
}
