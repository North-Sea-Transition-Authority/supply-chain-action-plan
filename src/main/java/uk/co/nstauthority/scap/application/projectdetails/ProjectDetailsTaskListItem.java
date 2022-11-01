package uk.co.nstauthority.scap.application.projectdetails;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Component;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewTaskListSection;
import uk.co.nstauthority.scap.application.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.tasklist.TaskListSection;

@Component
class ProjectDetailsTaskListItem implements ScapTaskListItem {
  private static final String DISPLAY_NAME = "Project details";
  private static final Integer DISPLAY_ORDER = 20;

  private final ScapOverviewService scapOverviewService;
  private final ScapDetailService scapDetailService;
  private final ProjectDetailsService projectDetailsService;
  private final ProjectDetailsFormService projectDetailsFormService;

  ProjectDetailsTaskListItem(ScapOverviewService scapOverviewService, ScapDetailService scapDetailService,
                             ProjectDetailsService projectDetailsService,
                             ProjectDetailsFormService projectDetailsFormService) {
    this.scapOverviewService = scapOverviewService;
    this.scapDetailService = scapDetailService;
    this.projectDetailsService = projectDetailsService;
    this.projectDetailsFormService = projectDetailsFormService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(target));
  }

  @Override
  public int getDisplayOrder() {
    return DISPLAY_ORDER;
  }

  @Override
  public boolean isValid(Integer target) {
    var scap = scapOverviewService.getScapById(target);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    return projectDetailsService.getProjectDetailsByScapDetail(scapDetail)
        .map(projectDetails -> {
          var form = projectDetailsFormService.getForm(projectDetails);
          var bindingResult = new BeanPropertyBindingResult(form, "form");
          return !projectDetailsFormService.validate(form, bindingResult).hasErrors();
        }).orElse(false);
  }

  @Override
  public Class<? extends TaskListSection<Integer>> getTaskListSection() {
    return ScapOverviewTaskListSection.class;
  }

  @Override
  public boolean isVisible(Integer target) {
    return true;
  }
}
