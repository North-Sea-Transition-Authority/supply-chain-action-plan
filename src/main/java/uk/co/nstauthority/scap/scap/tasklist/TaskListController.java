package uk.co.nstauthority.scap.scap.tasklist;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.delete.ScapDeletionController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.util.TaskListSectionUtil;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("/{scapId}/tasks")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class TaskListController {

  static final String WORK_AREA_URL = ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null));

  private final List<ScapTaskListSection> scapTaskListSections;
  private final List<ScapTaskListItem> scapTaskListItems;

  private final ScapDetailService scapDetailService;

  @Autowired
  public TaskListController(List<ScapTaskListSection> scapTaskListSections,
                            List<ScapTaskListItem> scapTaskListItems,
                            ScapDetailService scapDetailService) {
    this.scapTaskListSections = scapTaskListSections;
    this.scapTaskListItems = scapTaskListItems;
    this.scapDetailService = scapDetailService;
  }

  @GetMapping
  public ModelAndView renderTaskList(@PathVariable("scapId") ScapId scapId) {
    var deleteActionText = scapDetailService.isUpdateInProgress(scapId) ? "Delete draft update" : "Delete SCAP";

    return new ModelAndView("scap/scap/taskList")
        .addObject("backLinkUrl", WORK_AREA_URL)
        .addObject("taskListSections",
            TaskListSectionUtil.createSectionViews(scapTaskListSections,
                scapTaskListItems,
                scapId.scapId()))
        .addObject("deleteScapUrl",
            ReverseRouter.route(on(ScapDeletionController.class).renderScapDeletionConfirmation(scapId)))
        .addObject("deleteActionText", deleteActionText);
  }
}
