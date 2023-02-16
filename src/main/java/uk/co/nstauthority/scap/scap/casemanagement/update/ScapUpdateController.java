package uk.co.nstauthority.scap.scap.casemanagement.update;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequired;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Controller
@RequestMapping("{scapId}/")
@PermissionsRequired(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = {ScapDetailStatus.SUBMITTED, ScapDetailStatus.DRAFT})
public class ScapUpdateController {

  private final ScapDetailService scapDetailService;

  private final ScapService scapService;

  public ScapUpdateController(ScapDetailService scapDetailService, ScapService scapService) {
    this.scapDetailService = scapDetailService;
    this.scapService = scapService;
  }


  @PostMapping(params = CaseEventAction.UPDATE)
  public ModelAndView startScapUpdate(@PathVariable("scapId") ScapId scapId,
                                      @RequestParam(CaseEventAction.UPDATE) String caseEventAction) {
    var existingDraftUpdate = scapDetailService.findLatestByScapIdAndStatus(scapId, ScapDetailStatus.DRAFT);
    if (existingDraftUpdate.isEmpty()) {
      scapDetailService.createDraftScapDetail(scapService.getScapById(scapId));
    }
    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
  }
}
