package uk.co.nstauthority.scap.scap.actualtender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderActivitySummaryView;
import uk.co.nstauthority.scap.scap.summary.actualtender.ActualTenderSummaryViewService;

@Service
public class ActualTenderTaskListItemService {

  private final ActualTenderService actualTenderService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final ActualTenderSummaryViewService actualTenderSummaryViewService;

  @Autowired
  ActualTenderTaskListItemService(ActualTenderService actualTenderService,
                                  ActualTenderActivityService actualTenderActivityService,
                                  ActualTenderSummaryViewService actualTenderSummaryViewService) {
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderSummaryViewService = actualTenderSummaryViewService;
  }

  public boolean isValid(ScapDetail scapDetail) {
    var actualTenderOpt = actualTenderService.findByScapDetail(scapDetail);

    if (actualTenderOpt.isEmpty()) {
      return false;
    }

    var actualTender = actualTenderOpt.get();

    if (Boolean.FALSE.equals(actualTender.getHasActualTenders())) {
      return true;
    }

    var actualTenderActivities = actualTenderActivityService.getAllByActualTender(actualTender);

    if (actualTenderActivities.isEmpty()) {
      return false;
    }

    var actualTenderSummaryViews = actualTenderSummaryViewService
        .getByActualTenderActivities(actualTenderActivities, scapDetail.getScap().getScapId());
    if (!actualTenderSummaryViews.stream()
        .allMatch(ActualTenderActivitySummaryView::isValid)) {
      return false;
    }

    return HasMoreActualTenderActivities.NO.equals(actualTender.getHasMoreActualTenders());
  }
}
