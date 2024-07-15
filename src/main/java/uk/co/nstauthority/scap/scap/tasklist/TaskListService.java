package uk.co.nstauthority.scap.scap.tasklist;

import jakarta.transaction.Transactional;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.tasklist.TaskListSection;
import uk.co.nstauthority.scap.tasklist.TaskListSectionService;

@Service
class TaskListService {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final List<TaskListSectionService> sectionServices;

  @Autowired
  TaskListService(ScapService scapService, ScapDetailService scapDetailService,
                  List<TaskListSectionService> sectionServices) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.sectionServices = sectionServices;
  }

  @Transactional
  public List<TaskListSection> getTaskListSections(ScapId scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestByScap(scap);

    return sectionServices.stream()
        .map(service -> service.getSection(scapDetail))
        .flatMap(Optional::stream)
        .sorted(Comparator.comparing(TaskListSection::displayOrder))
        .toList();
  }
}
