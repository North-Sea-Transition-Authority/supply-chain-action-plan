package uk.co.nstauthority.scap.scap.projectdetails;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
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
class ProjectDetailsTaskListItem implements ScapTaskListItem {
  private static final String DISPLAY_NAME = "Project details";
  private static final Integer DISPLAY_ORDER = 20;

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ProjectDetailsService projectDetailsService;
  private final ProjectDetailsFormService projectDetailsFormService;

  ProjectDetailsTaskListItem(ScapService scapService, ScapDetailService scapDetailService,
                             ProjectDetailsService projectDetailsService,
                             ProjectDetailsFormService projectDetailsFormService) {
    this.scapService = scapService;
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
    return ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(new ScapId(target)));
  }

  @Override
  public int getDisplayOrder() {
    return DISPLAY_ORDER;
  }

  @Override
  public boolean isValid(Integer target) {
    var scap = scapService.getScapById(target);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var projectDetailsOptional = projectDetailsService.getProjectDetails(scapDetail);
    var projectFacilities = projectDetailsOptional
        .map(projectDetailsService::getProjectFacilities)
        .orElse(Collections.emptyList());
    var projectFields = projectDetailsOptional
        .map(projectDetailsService::getProjectFields)
        .orElse(Collections.emptyList());
    var projectFacilityIds = projectFacilities
        .stream()
        .map(ProjectFacility::getFacilityId)
        .toList();
    var projectFieldIds = projectFields
        .stream()
        .map(ProjectField::getFieldId)
        .toList();
    return projectDetailsOptional
        .map(projectDetails -> {
          var form = projectDetailsFormService
              .getForm(projectDetails, projectFacilityIds, projectFieldIds);
          var bindingResult = new BeanPropertyBindingResult(form, "form");
          return !projectDetailsFormService.validate(form, bindingResult).hasErrors();
        }).orElse(false);
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
