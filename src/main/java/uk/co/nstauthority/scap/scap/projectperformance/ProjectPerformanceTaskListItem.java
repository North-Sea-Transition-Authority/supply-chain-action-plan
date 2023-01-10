package uk.co.nstauthority.scap.scap.projectperformance;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import com.google.common.annotations.VisibleForTesting;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
public class ProjectPerformanceTaskListItem implements ScapTaskListItem {

  @VisibleForTesting
  static final String DISPLAY_TEXT = "Project performance and close-out";

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ProjectPerformanceService projectPerformanceService;
  private final ProjectPerformanceFormService projectPerformanceFormService;

  @Autowired
  ProjectPerformanceTaskListItem(ScapService scapService, ScapDetailService scapDetailService,
                                 ProjectPerformanceService projectPerformanceService,
                                 ProjectPerformanceFormService projectPerformanceFormService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.projectPerformanceService = projectPerformanceService;
    this.projectPerformanceFormService = projectPerformanceFormService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_TEXT;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(new ScapId(target)));
  }

  @Override
  public int getDisplayOrder() {
    return 60;
  }

  @Override
  public boolean isVisible(Integer target) {
    return true;
  }

  @Override
  public boolean isValid(Integer target) {
    var scap = scapService.getScapById(target);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var projectPerformance = projectPerformanceService.findByScapDetail(scapDetail);
    return projectPerformance
        .map(existingProjectPerformance -> {
          var form = projectPerformanceFormService.getForm(existingProjectPerformance);
          var bindingResult = new BeanPropertyBindingResult(form, "form");
          bindingResult = (BeanPropertyBindingResult) projectPerformanceFormService.validate(form, bindingResult);
          return !bindingResult.hasErrors();
        })
        .orElse(false);
  }

  @Override
  public Class<? extends TaskListSection<Integer>> getTaskListSection() {
    return ScapFormTaskListSection.class;
  }
}
