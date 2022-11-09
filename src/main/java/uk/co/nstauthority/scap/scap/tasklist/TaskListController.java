package uk.co.nstauthority.scap.scap.tasklist;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.tasklist.TaskListSectionUtil;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("/{scapOverviewId}/tasks")
public class TaskListController {

  private final List<ScapTaskListSection> scapTaskListSections;
  private final List<ScapTaskListItem> scapTaskListItems;

  @Autowired
  public TaskListController(List<ScapTaskListSection> scapTaskListSections, List<ScapTaskListItem> scapTaskListItems) {
    this.scapTaskListSections = scapTaskListSections;
    this.scapTaskListItems = scapTaskListItems;
  }

  @GetMapping
  public ModelAndView renderTaskList(@PathVariable("scapOverviewId") Integer scapId) {

    return new ModelAndView("scap/application/taskList")
        .addObject("backLinkUrl", ReverseRouter.route(on(WorkAreaController.class).getWorkArea()))
        .addObject("taskListSections", TaskListSectionUtil.createSectionViews(scapTaskListSections, scapTaskListItems, scapId));
  }

}
