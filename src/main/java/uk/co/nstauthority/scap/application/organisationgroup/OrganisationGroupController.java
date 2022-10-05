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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
public class OrganisationGroupController {

  private final String NEW_SCAP_BACK_LINK_URL =
      ReverseRouter.route(on(ScapStartController.class).renderStartNewScap());
  private final String NEW_SCAP_POST_URL =
      ReverseRouter.route(on(OrganisationGroupController.class).saveNewScapOrganisationGroup(null, emptyBindingResult()));
  // TODO SCAP2022-29: Replace work area link with link to task list for this SCAP
  private final String EXISTING_SCAP_BACK_LINK_URL =
      ReverseRouter.route(on(WorkAreaController.class).getWorkArea());
  private final String ORGANISATION_GROUP_SEARCH_REST_URL =
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
    return organisationGroupFormModelAndView(NEW_SCAP_BACK_LINK_URL, NEW_SCAP_POST_URL);
  }

  @PostMapping("/new/organisation-group")
  public ModelAndView saveNewScapOrganisationGroup(@ModelAttribute("form") OrganisationGroupForm form,
                                                   BindingResult bindingResult) {
    bindingResult = organisationGroupFormService.validate(form, bindingResult);
    if (bindingResult.hasErrors()) {
      return organisationGroupFormModelAndView(NEW_SCAP_BACK_LINK_URL, NEW_SCAP_POST_URL)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    scapOverviewService.createScapOverview(Integer.valueOf(form.getOrganisationGroupId().getInputValue()));
    // TODO SCAP2022-29: Redirect to task list on save
    return ReverseRouter.redirect(on(OrganisationGroupController.class).renderNewScapOrganisationGroupForm(null));
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

    return organisationGroupFormModelAndView(EXISTING_SCAP_BACK_LINK_URL, postUrl, preselectedItems)
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
      return organisationGroupFormModelAndView(EXISTING_SCAP_BACK_LINK_URL, postUrl)
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    scapOverviewService.updateScapOverviewOrganisationGroup(scapOverview,
        Integer.valueOf(form.getOrganisationGroupId().getInputValue()));
    // TODO SCAP2022-29: Redirect to task list on save
    return ReverseRouter.redirect(on(OrganisationGroupController.class)
        .renderExistingScapOrganisationGroupForm(scapOverviewId));
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
        .addObject("organisationGroupSearchRestUrl", ORGANISATION_GROUP_SEARCH_REST_URL)
        .addObject("preselectedItems", preselectedItems);
  }
}
