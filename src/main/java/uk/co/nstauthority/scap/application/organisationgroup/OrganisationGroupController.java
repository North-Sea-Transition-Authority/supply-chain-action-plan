package uk.co.nstauthority.scap.application.organisationgroup;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

import java.util.Collections;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.start.ScapStartController;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@Controller
public class OrganisationGroupController {

  private final String newScapBackLinkUrl =
      ReverseRouter.route(on(ScapStartController.class).renderStartNewScap());
  private final String newScapPostUrl =
      ReverseRouter.route(on(OrganisationGroupController.class).saveNewScapOrganisationGroup(null, emptyBindingResult()));
  private final String organisationGroupSearchRestUrl =
      ReverseRouter.route(on(OrganisationGroupRestController.class).getOrganisationGroupSearchResults(null));

  private final ScapOverviewService scapOverviewService;
  private final OrganisationGroupFormService organisationGroupFormService;
  private final ValidationErrorOrderingService validationErrorOrderingService;

  private final OrganisationGroupService organisationGroupService;

  @Autowired
  public OrganisationGroupController(ScapOverviewService scapOverviewService,
                                     OrganisationGroupFormService organisationGroupFormService,
                                     ValidationErrorOrderingService validationErrorOrderingService,
                                     OrganisationGroupService organisationGroupService) {
    this.scapOverviewService = scapOverviewService;
    this.organisationGroupFormService = organisationGroupFormService;
    this.validationErrorOrderingService = validationErrorOrderingService;
    this.organisationGroupService = organisationGroupService;
  }

  @GetMapping("/new/organisation-group")
  public ModelAndView renderNewScapOrganisationGroupForm(@ModelAttribute("form") OrganisationGroupForm form) {
    return organisationGroupFormModelAndView(newScapBackLinkUrl, newScapPostUrl);
  }

  @PostMapping("/new/organisation-group")
  public ModelAndView saveNewScapOrganisationGroup(@ModelAttribute("form") OrganisationGroupForm form,
                                                   BindingResult bindingResult) {
    bindingResult = organisationGroupFormService.validate(form, bindingResult);
    if (bindingResult.hasErrors()) {
      return organisationGroupFormModelAndView(newScapBackLinkUrl, newScapPostUrl)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    var scap = scapOverviewService.createScapOverview(Integer.valueOf(form.getOrganisationGroupId().getInputValue()));
    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scap.getId()));
  }

  @GetMapping("/{scapOverviewId}/organisation-group")
  public ModelAndView renderExistingScapOrganisationGroupForm(@PathVariable("scapOverviewId") Integer scapOverviewId) {
    var scapOverview = scapOverviewService.getScapOverviewById(scapOverviewId);
    var form = organisationGroupFormService.getForm(scapOverview);
    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveExistingScapOrganisationGroup(null, scapOverviewId, emptyBindingResult()));
    var preselectedItems = organisationGroupService.getOrganisationGroupById(
        scapOverview.getOrganisationGroupId(), "Get name of current SCAP operator")
            .map(organisationGroup -> Map.of(organisationGroup.getOrganisationGroupId(), organisationGroup.getName()))
            .orElse(Collections.emptyMap());
    var existingScapBackLinkUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scapOverviewId));

    return organisationGroupFormModelAndView(existingScapBackLinkUrl, postUrl, preselectedItems)
        .addObject("form", form);
  }

  @PostMapping("/{scapOverviewId}/organisation-group")
  public ModelAndView saveExistingScapOrganisationGroup(@ModelAttribute("form") OrganisationGroupForm form,
                                                        @PathVariable("scapOverviewId") Integer scapOverviewId,
                                                        BindingResult bindingResult) {
    var scapOverview = scapOverviewService.getScapOverviewById(scapOverviewId);
    bindingResult = organisationGroupFormService.validate(form, bindingResult);
    if (bindingResult.hasErrors()) {
      var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
          .saveExistingScapOrganisationGroup(null, scapOverviewId, emptyBindingResult()));
      var existingScapBackLinkUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scapOverviewId));
      return organisationGroupFormModelAndView(existingScapBackLinkUrl, postUrl)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    scapOverviewService.updateScapOverviewOrganisationGroup(scapOverview,
        Integer.valueOf(form.getOrganisationGroupId().getInputValue()));
    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapOverviewId));
  }

  private ModelAndView organisationGroupFormModelAndView(String backLinkUrl, String postUrl) {
    return organisationGroupFormModelAndView(backLinkUrl, postUrl, Collections.emptyMap());
  }

  private ModelAndView organisationGroupFormModelAndView(String backLinkUrl,
                                                         String postUrl,
                                                         Map<Integer, String> preselectedItems) {
    return new ModelAndView("scap/application/organisationGroup")
        .addObject("backLinkUrl", backLinkUrl)
        .addObject("submitPostUrl", postUrl)
        .addObject("organisationGroupSearchRestUrl", organisationGroupSearchRestUrl)
        .addObject("preselectedItems", preselectedItems);
  }
}
