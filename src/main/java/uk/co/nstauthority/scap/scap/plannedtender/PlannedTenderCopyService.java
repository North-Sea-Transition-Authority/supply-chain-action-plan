package uk.co.nstauthority.scap.scap.plannedtender;

import static uk.co.nstauthority.scap.scap.detail.NewScapType.DRAFT_UPDATE;

import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.copy.CopyService;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;

@Service
public class PlannedTenderCopyService implements CopyService {

  private final PlannedTenderRepository plannedTenderRepository;

  private final PlannedTenderService plannedTenderService;

  private final PlannedTenderActivityService plannedTenderActivityService;

  private final EntityCopyService entityCopyService;

  public PlannedTenderCopyService(PlannedTenderRepository plannedTenderRepository,
                                  PlannedTenderService plannedTenderService,
                                  PlannedTenderActivityService plannedTenderActivityService,
                                  EntityCopyService entityCopyService) {
    this.plannedTenderRepository = plannedTenderRepository;
    this.plannedTenderService = plannedTenderService;
    this.plannedTenderActivityService = plannedTenderActivityService;
    this.entityCopyService = entityCopyService;
  }

  @Override
  public int runOrder() {
    return 10;
  }

  @Override
  public void copyEntity(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType newScapType) {
    var oldPlannedTender = plannedTenderService.getByScapDetail(oldScapDetail);
    var newPlannedTender = (PlannedTender) entityCopyService.copyChild(newScapDetail, oldPlannedTender);

    if (DRAFT_UPDATE.equals(newScapType)) {
      if (!Boolean.TRUE.equals(oldPlannedTender.getHasPlannedTenders())) {
        newPlannedTender.setHasPlannedTenders(null);
      }
      newPlannedTender.setHasMorePlannedTenderActivities(null);
      plannedTenderRepository.save(newPlannedTender);
    }

    var oldPlannedActivities = plannedTenderActivityService.getTenderDetailsByPlannedTender(oldPlannedTender);
    entityCopyService.copyChildren(newPlannedTender, oldPlannedActivities);
  }
}
