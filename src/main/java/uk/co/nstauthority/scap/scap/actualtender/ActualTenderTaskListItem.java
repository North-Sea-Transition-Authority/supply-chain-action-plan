package uk.co.nstauthority.scap.scap.actualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class ActualTenderTaskListItem implements ScapTaskListItem {

  private static final String DISPLAY_NAME = "Actual tendering activity";

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ActualTenderService actualTenderService;

  @Autowired
  public ActualTenderTaskListItem(ScapService scapService, ScapDetailService scapDetailService,
                                  ActualTenderService actualTenderService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.actualTenderService = actualTenderService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer scapId) {
    return ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));
  }

  @Override
  public int getDisplayOrder() {
    return 40;
  }

  @Override
  public boolean isValid(Integer scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var actualTender = actualTenderService.getByScapDetail(scapDetail);
    return actualTender.map(existingActualTender ->
        Boolean.FALSE.equals(existingActualTender.getHasActualTenders())
        || Boolean.TRUE.equals(existingActualTender.getAllActualTendersAdded()))
        .orElse(false);
  }

  @Override
  public Class<? extends TaskListSection<Integer>> getTaskListSection() {
    return ScapFormTaskListSection.class;
  }

  @Override
  public boolean isVisible(Integer target) {
    return true;
  }
}
