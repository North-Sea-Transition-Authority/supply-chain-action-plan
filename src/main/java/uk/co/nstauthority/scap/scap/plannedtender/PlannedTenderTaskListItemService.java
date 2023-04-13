package uk.co.nstauthority.scap.scap.plannedtender;

import static uk.co.nstauthority.scap.util.TaskListItemUtil.getBindingResultForForm;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityFormService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderFormService;

@Service
public class PlannedTenderTaskListItemService {

  private final PlannedTenderService plannedTenderService;
  private final HasPlannedTenderFormService hasPlannedTenderFormService;
  private final PlannedTenderActivityService plannedTenderActivityService;

  private final PlannedTenderActivityFormService plannedTenderActivityFormService;

  @Autowired
  public PlannedTenderTaskListItemService(PlannedTenderService plannedTenderService,
                                          HasPlannedTenderFormService hasPlannedTenderFormService,
                                          PlannedTenderActivityService plannedTenderActivityService,
                                          PlannedTenderActivityFormService plannedTenderActivityFormService) {
    this.plannedTenderService = plannedTenderService;
    this.hasPlannedTenderFormService = hasPlannedTenderFormService;
    this.plannedTenderActivityService = plannedTenderActivityService;
    this.plannedTenderActivityFormService = plannedTenderActivityFormService;
  }

  public boolean isValid(ScapDetail scapDetail) {
    var plannedTenderOpt = plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);

    if (plannedTenderOpt.isEmpty()) {
      return false;
    }

    var plannedTender = plannedTenderOpt.get();
    var hasPlannedTenderForm = hasPlannedTenderFormService.scapPlannedTenderToForm(plannedTender);
    var hasPlannedTenderFormBindingResult = hasPlannedTenderFormService
        .validate(hasPlannedTenderForm, getBindingResultForForm(hasPlannedTenderForm));

    if (hasPlannedTenderFormBindingResult.hasErrors()) {
      return false;
    }

    if (YesNo.NO.equals(hasPlannedTenderForm.getHasPlannedTender())) {
      return true;
    }

    var plannedTenderActivities = plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender);

    if (plannedTenderActivities.isEmpty()) {
      return false;
    }

    var anyActivitiesInvalid = plannedTenderActivities.stream()
        .map(plannedTenderActivityFormService::getForm)
        .anyMatch(form -> plannedTenderActivityFormService.validate(getBindingResultForForm(form), form).hasErrors());

    if (anyActivitiesInvalid) {
      return false;
    }

    return HasMorePlannedTenderActivities.NO.equals(plannedTender.getHasMorePlannedTenderActivities());
  }
}
