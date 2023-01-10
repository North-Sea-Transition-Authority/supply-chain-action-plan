package uk.co.nstauthority.scap.scap.timeline;

import java.util.List;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TimelineEventRepository extends CrudRepository<TimelineEvent, Integer> {

  List<TimelineEvent> findAllByScapId(Integer scapId);
}
