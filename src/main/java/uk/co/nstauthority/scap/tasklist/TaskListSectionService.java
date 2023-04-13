package uk.co.nstauthority.scap.tasklist;

import java.util.Optional;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

public interface TaskListSectionService {

  Optional<TaskListSection> getSection(ScapDetail scapDetail);

}
