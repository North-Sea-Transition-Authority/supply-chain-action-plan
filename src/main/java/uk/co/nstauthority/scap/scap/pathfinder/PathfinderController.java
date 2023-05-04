package uk.co.nstauthority.scap.scap.pathfinder;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.energyportal.rest.PathfinderRestController;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/pathfinder-projects")
@HasAnyPermissionForScap(permissions = RolePermission.SUBMIT_SCAP)
public class PathfinderController {


  private final ControllerHelperService controllerHelperService;
  private final ScapDetailService scapDetailService;
  private final PathfinderService pathfinderService;
  private final PathfinderFormValidator pathfinderFormValidator;
  private final PathfinderProjectService pathfinderProjectService;

  @Autowired
  PathfinderController(ControllerHelperService controllerHelperService,
                       ScapDetailService scapDetailService,
                       PathfinderService pathfinderService,
                       PathfinderFormValidator pathfinderFormValidator,
                       PathfinderProjectService pathfinderProjectService) {
    this.controllerHelperService = controllerHelperService;
    this.scapDetailService = scapDetailService;
    this.pathfinderService = pathfinderService;
    this.pathfinderFormValidator = pathfinderFormValidator;
    this.pathfinderProjectService = pathfinderProjectService;
  }

  @GetMapping
  public ModelAndView renderPathfinderProjectsForm(@PathVariable("scapId") ScapId scapId) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);
    var pathfinderProjectsOverviewOptional = pathfinderService.findPathfinderProjectsOverview(scapDetail);
    var pathfinderProjects = pathfinderProjectsOverviewOptional
        .map(pathfinderService::findAllByPathfinderProjectsOverview)
        .orElse(Collections.emptySet());
    var preselectedProjects = pathfinderProjectService.getProjectAddToListItems(pathfinderProjects.stream()
        .map(PathfinderProject::getPathfinderProjectId)
        .toList());
    var form = pathfinderProjectsOverviewOptional
        .map(pathfinderProjectsOverview -> PathfinderForm.from(pathfinderProjectsOverview, pathfinderProjects))
        .orElse(new PathfinderForm());

    return pathfinderProjectsFormModelAndView(scapId, preselectedProjects)
        .addObject("form", form);
  }

  @PostMapping
  ModelAndView savePathfinderProjectsForm(@PathVariable("scapId") ScapId scapId,
                                          @ModelAttribute("form") PathfinderForm form) {
    var bindingResult = pathfinderFormValidator.validate(form);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        pathfinderProjectsFormModelAndView(scapId, Collections.emptyList()),
        form,
        () -> {
          var scapDetail = scapDetailService.getLatestByScapId(scapId);
          pathfinderService.saveRelatedPathfinderProjects(scapDetail, form);
          return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
        }
    );
  }

  private ModelAndView pathfinderProjectsFormModelAndView(ScapId scapId, List<AddToListItem> preselectedProjects) {
    return new ModelAndView("scap/scap/pathfinder")
        .addObject("backLinkUrl", ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId)))
        .addObject("pathfinderSearchRestUrl",
            ReverseRouter.route(on(PathfinderRestController.class).getPathfinderSearchResults(scapId, null)))
        .addObject("preselectedProjects", preselectedProjects);
  }
}
