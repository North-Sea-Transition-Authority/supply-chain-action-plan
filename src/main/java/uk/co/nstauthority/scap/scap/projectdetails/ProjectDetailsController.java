package uk.co.nstauthority.scap.scap.projectdetails;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.fds.searchselector.SearchSelectorService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/project-details")
class ProjectDetailsController {

  private final ProjectDetailsFormService projectDetailsFormService;
  private final ControllerHelperService controllerHelperService;
  private final ScapService scapService;
  private final ScapDetailService scapDetailService;
  private final ProjectDetailsService projectDetailsService;

  @Autowired
  ProjectDetailsController(ProjectDetailsFormService projectDetailsFormService,
                           ControllerHelperService controllerHelperService, ScapService scapService,
                           ScapDetailService scapDetailService, ProjectDetailsService projectDetailsService) {
    this.projectDetailsFormService = projectDetailsFormService;
    this.controllerHelperService = controllerHelperService;
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
    this.projectDetailsService = projectDetailsService;
  }

  @GetMapping
  ModelAndView renderProjectDetailsForm(@PathVariable("scapId") Integer scapId) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var form = projectDetailsService.getProjectDetailsByScapDetail(scapDetail)
        .map(projectDetailsFormService::getForm)
        .orElse(new ProjectDetailsForm());
    var preselectedField = form.getFieldId().getAsInteger()
        .flatMap(projectDetailsFormService::getPreselectedField)
        .orElse(null);
    return projectDetailsFormModelAndView(scapId, form, preselectedField);
  }

  @PostMapping
  ModelAndView saveProjectDetailsForm(@PathVariable("scapId") Integer scapId,
                                      @ModelAttribute("form") ProjectDetailsForm form,
                                      BindingResult bindingResult) {
    var scap = scapService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var preselectedField = form.getFieldId().getAsInteger()
        .flatMap(projectDetailsFormService::getPreselectedField)
        .orElse(null);
    bindingResult = projectDetailsFormService.validate(form, bindingResult);
    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        projectDetailsFormModelAndView(scapId, form, preselectedField),
        form,
        () -> {
          projectDetailsService.saveProjectDetails(scapDetail, form);
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        });
  }

  private ModelAndView projectDetailsFormModelAndView(Integer scapId, ProjectDetailsForm form,
                                                      @Nullable Map<String, String> preselectedField) {
    return new ModelAndView("scap/application/projectDetails")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("fieldSearchRestUrl",
            SearchSelectorService.route(on(ProjectDetailsRestController.class)
                .getFieldSearchResults(null)))
        .addObject("projectTypesMap", ProjectType.getCheckboxItems())
        .addObject("form", form)
        .addObject("preselectedField", preselectedField);
  }
}
