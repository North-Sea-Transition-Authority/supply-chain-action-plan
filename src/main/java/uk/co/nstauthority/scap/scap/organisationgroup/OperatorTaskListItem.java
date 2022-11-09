package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.scap.ScapFormTaskListSection;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.ScapTaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListSection;


@Component
public class OperatorTaskListItem implements ScapTaskListItem {

  private static final String DISPLAY_NAME = "SCAP operator";
  private final ScapService scapService;
  private final OrganisationGroupFormService organisationGroupFormService;

  @Autowired
  public OperatorTaskListItem(ScapService scapService,
                              OrganisationGroupFormService organisationGroupFormService) {
    this.scapService = scapService;
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
    var scap = scapService.getScapById(target);
    var form = organisationGroupFormService.getForm(scap);
    var bindingResult = organisationGroupFormService.validate(form, new BeanPropertyBindingResult(form, "form"));
    return !bindingResult.hasErrors();
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
