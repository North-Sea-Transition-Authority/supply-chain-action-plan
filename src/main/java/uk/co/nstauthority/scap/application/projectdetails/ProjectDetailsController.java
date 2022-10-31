package uk.co.nstauthority.scap.application.projectdetails;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.tasklist.TaskListController;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.fds.searchselector.SearchSelectorService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("{scapId}/project-details")
class ProjectDetailsController {

  private final ProjectDetailsFormService projectDetailsFormService;
  private final ControllerHelperService controllerHelperService;

  @Autowired
  ProjectDetailsController(ProjectDetailsFormService projectDetailsFormService,
                           ControllerHelperService controllerHelperService) {
    this.projectDetailsFormService = projectDetailsFormService;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping
  ModelAndView renderProjectDetailsForm(@PathVariable("scapId") Integer scapId,
                                        @ModelAttribute("form") ProjectDetailsForm form) {
    return projectDetailsFormModelAndView(scapId);
  }

  @PostMapping
  ModelAndView saveProjectDetailsForm(@PathVariable("scapId") Integer scapId,
                                      @ModelAttribute("form") ProjectDetailsForm form,
                                      BindingResult bindingResult) {
    bindingResult = projectDetailsFormService.validate(form, bindingResult);
    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        projectDetailsFormModelAndView(scapId),
        form,
        () -> {
          // TODO: This will be added to in next PR with data model and saving
          return ReverseRouter.redirect(on(ProjectDetailsController.class).renderProjectDetailsForm(scapId, null));
        });
  }

  private ModelAndView projectDetailsFormModelAndView(Integer scapId) {
    return new ModelAndView("scap/application/projectDetails")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("fieldSearchRestUrl",
            SearchSelectorService.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null)))
        .addObject("projectTypesMap", ProjectType.getCheckboxItems());
  }
}
