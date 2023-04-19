package uk.co.nstauthority.scap.scap.projectperformance;

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
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/project-performance")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class ProjectPerformanceController {

  private final ScapDetailService scapDetailService;
  private final ProjectPerformanceService projectPerformanceService;
  private final ProjectPerformanceFormService projectPerformanceFormService;
  private final ControllerHelperService controllerHelperService;

  @Autowired
  ProjectPerformanceController(ScapDetailService scapDetailService,
                               ProjectPerformanceFormService projectPerformanceFormService,
                               ProjectPerformanceService projectPerformanceService,
                               ControllerHelperService controllerHelperService) {
    this.scapDetailService = scapDetailService;
    this.projectPerformanceFormService = projectPerformanceFormService;
    this.projectPerformanceService = projectPerformanceService;
    this.controllerHelperService = controllerHelperService;
  }

  @GetMapping
  public ModelAndView renderProjectPerformanceForm(@PathVariable("scapId") ScapId scapId) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);
    var projectPerformance = projectPerformanceService.findByScapDetail(scapDetail);

    var form = projectPerformance.map(projectPerformanceFormService::getForm)
        .orElse(new ProjectPerformanceForm());

    return projectPerformanceModelAndView(scapId)
        .addObject("form", form);
  }

  @PostMapping
  public ModelAndView saveProjectPerformanceForm(@PathVariable("scapId") ScapId scapId,
                                                 @ModelAttribute("form") ProjectPerformanceForm form,
                                                 BindingResult bindingResult) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);
    var projectPerformance = projectPerformanceService.findByScapDetail(scapDetail);

    bindingResult = projectPerformanceFormService.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        projectPerformanceModelAndView(scapId),
        form,
        () -> {
          projectPerformance.ifPresentOrElse(
              existingProjectPerformance -> projectPerformanceService
                  .updateProjectPerformance(existingProjectPerformance, form),
              () -> projectPerformanceService.createProjectPerformance(scapDetail, form));
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        }
    );
  }

  private ModelAndView projectPerformanceModelAndView(ScapId scapId) {
    return new ModelAndView("scap/scap/projectPerformance")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("radioItems", YesNo.getRadioOptions());
  }
}
