package uk.co.nstauthority.scap.application.organisationgroup;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewTaskListSection;
import uk.co.nstauthority.scap.application.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.tasklist.TaskListSection;


@Component
public class OperatorTaskListItem implements ScapTaskListItem {

  private static final String DISPLAY_NAME = "SCAP operator";
  private final ScapOverviewService scapOverviewService;
  private final OrganisationGroupFormService organisationGroupFormService;

  @Autowired
  public OperatorTaskListItem(ScapOverviewService scapOverviewService,
                              OrganisationGroupFormService organisationGroupFormService) {
    this.scapOverviewService = scapOverviewService;
    this.organisationGroupFormService = organisationGroupFormService;
  }

  @Override
  public String getItemDisplayText() {
    return DISPLAY_NAME;
  }

  @Override
  public String getActionUrl(Integer target) {
    return ReverseRouter.route(on(OrganisationGroupController.class).renderExistingScapOrganisationGroupForm(target));
  }

  @Override
  public int getDisplayOrder() {
    return 10;
  }

  @Override
  public boolean isValid(Integer target) {
    var scap = scapOverviewService.getScapOverviewById(target);
    var form = organisationGroupFormService.getForm(scap);
    var bindingResult = organisationGroupFormService.validate(form, new BeanPropertyBindingResult(form, "form"));
    return !bindingResult.hasErrors();
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
