package uk.co.nstauthority.scap.scap.scap;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.util.TaskListItemUtil.getBindingResultForForm;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderTaskListItemService;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceTaskListItemService;
import uk.co.nstauthority.scap.scap.contractingperformance.hascontractingperformance.HasContractingPerformanceController;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupController;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupFormService;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderController;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderForm;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderFormValidator;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderService;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderTaskListItemService;
import uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender.HasPlannedTenderController;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsController;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsFormService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectFacility;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectField;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceController;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceFormService;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformanceService;
import uk.co.nstauthority.scap.tasklist.TaskListItem;
import uk.co.nstauthority.scap.tasklist.TaskListLabel;
import uk.co.nstauthority.scap.tasklist.TaskListSection;
import uk.co.nstauthority.scap.tasklist.TaskListSectionService;

@Component
public class ScapFormTaskListSectionService implements TaskListSectionService {

  static final String DISPLAY_NAME = "Overview";
  static final Integer DISPLAY_ORDER = 1;

  static final String OPERATOR_DISPLAY_NAME = "SCAP operator";
  static final String PROJECT_DETAILS_DISPLAY_NAME = "Project details";
  static final String PLANNED_TENDER_DISPLAY_NAME = "Planned tender activity";
  static final String PATHFINDER_DISPLAY_NAME = "Related Pathfinder projects";
  static final String ACTUAL_TENDER_DISPLAY_NAME = "Actual tender activity";
  static final String CONTRACTING_PERFORMANCE_DISPLAY_NAME = "Contracting performance";
  static final String PROJECT_PERFORMANCE_DISPLAY_NAME = "Project performance and close-out";

  private final ProjectDetailsService projectDetailsService;
  private final ProjectDetailsFormService projectDetailsFormService;
  private final PlannedTenderTaskListItemService plannedTenderTaskListItemService;
  private final ActualTenderTaskListItemService actualTenderTaskListItemService;
  private final ContractingPerformanceTaskListItemService contractingPerformanceTaskListItemService;
  private final ProjectPerformanceService projectPerformanceService;
  private final ProjectPerformanceFormService projectPerformanceFormService;
  private final OrganisationGroupFormService organisationGroupFormService;
  private final PathfinderService pathfinderService;
  private final PathfinderFormValidator pathfinderFormValidator;

  @Autowired
  public ScapFormTaskListSectionService(ProjectDetailsService projectDetailsService,
                                        ProjectDetailsFormService projectDetailsFormService,
                                        PlannedTenderTaskListItemService plannedTenderTaskListItemService,
                                        ActualTenderTaskListItemService actualTenderTaskListItemService,
                                        ContractingPerformanceTaskListItemService contractingPerformanceTaskListItemService,
                                        ProjectPerformanceService projectPerformanceService,
                                        ProjectPerformanceFormService projectPerformanceFormService,
                                        OrganisationGroupFormService organisationGroupFormService,
                                        PathfinderService pathfinderService,
                                        PathfinderFormValidator pathfinderFormValidator) {
    this.projectDetailsService = projectDetailsService;
    this.projectDetailsFormService = projectDetailsFormService;
    this.plannedTenderTaskListItemService = plannedTenderTaskListItemService;
    this.actualTenderTaskListItemService = actualTenderTaskListItemService;
    this.contractingPerformanceTaskListItemService = contractingPerformanceTaskListItemService;
    this.projectPerformanceService = projectPerformanceService;
    this.projectPerformanceFormService = projectPerformanceFormService;
    this.organisationGroupFormService = organisationGroupFormService;
    this.pathfinderService = pathfinderService;
    this.pathfinderFormValidator = pathfinderFormValidator;
  }

  @Override
  public Optional<TaskListSection> getSection(ScapDetail scapDetail) {
    var scap = scapDetail.getScap();
    var scapId = scap.getScapId();
    var taskListItems = List.of(
        getScapOperatorTaskListItem(scapId, scapDetail),
        getProjectDetailsTaskListItem(scapId, scapDetail),
        getPlannedTenderTaskListItem(scapId, scapDetail),
        getPathfinderTaskListItem(scapId, scapDetail),
        getActualTenderTaskListItem(scapId, scapDetail),
        getContractingPerformanceTaskListItem(scapId, scapDetail),
        getProjectPerformanceTaskListItem(scapId, scapDetail)
    );

    return Optional.of(new TaskListSection(DISPLAY_NAME, DISPLAY_ORDER, taskListItems));
  }

  /* SCAP operator */
  TaskListItem getScapOperatorTaskListItem(ScapId scapId, ScapDetail scapDetail) {
    var actionUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .renderExistingScapOrganisationGroupForm(scapId));
    var form = organisationGroupFormService.getForm(scapDetail);
    var bindingResult = organisationGroupFormService.validate(form, getBindingResultForForm(form));

    return new TaskListItem(
        OPERATOR_DISPLAY_NAME,
        TaskListLabel.from(!bindingResult.hasErrors()),
        actionUrl
    );
  }

  /* Project details */
  TaskListItem getProjectDetailsTaskListItem(ScapId scapId, ScapDetail scapDetail) {
    var actionUrl = ReverseRouter.route(on(ProjectDetailsController.class).renderProjectDetailsForm(scapId));
    var projectDetailsOpt = projectDetailsService.findByScapDetail(scapDetail);

    var isValid = projectDetailsOpt.map(projectDetails -> {
      var projectFacilityIds = projectDetailsService.getProjectFacilities(projectDetails)
          .stream()
          .map(ProjectFacility::getFacilityId)
          .collect(Collectors.toSet());
      var projectFieldIds = projectDetailsService.getProjectFields(projectDetails)
          .stream()
          .map(ProjectField::getFieldId)
          .collect(Collectors.toSet());
      var form = projectDetailsFormService.getForm(projectDetails, projectFacilityIds, projectFieldIds);
      var bindingResult = projectDetailsFormService.validate(form, getBindingResultForForm(form));
      return !bindingResult.hasErrors();
    }).orElse(false);

    return new TaskListItem(
        PROJECT_DETAILS_DISPLAY_NAME,
        TaskListLabel.from(isValid),
        actionUrl
    );
  }

  /* Planned tender activity */
  TaskListItem getPlannedTenderTaskListItem(ScapId scapId, ScapDetail scapDetail) {
    var actionUrl = ReverseRouter.route(on(HasPlannedTenderController.class)
        .renderHasPlannedTenderActivityForm(scapId));
    var isValid = plannedTenderTaskListItemService.isValid(scapDetail);

    return new TaskListItem(
        PLANNED_TENDER_DISPLAY_NAME,
        TaskListLabel.from(isValid),
        actionUrl
    );
  }

  /* Actual tender activity */
  TaskListItem getActualTenderTaskListItem(ScapId scapId, ScapDetail scapDetail) {
    var actionUrl = ReverseRouter.route(on(HasActualTenderController.class)
        .renderHasActualTenderForm(scapId));
    var isValid = actualTenderTaskListItemService.isValid(scapDetail);

    return new TaskListItem(
        ACTUAL_TENDER_DISPLAY_NAME,
        TaskListLabel.from(isValid),
        actionUrl
    );
  }

  /* Contracting performance */
  TaskListItem getContractingPerformanceTaskListItem(ScapId scapId, ScapDetail scapDetail) {
    var actionUrl = ReverseRouter.route(on(HasContractingPerformanceController.class)
        .renderHasContractingPerformanceForm(scapId));
    var isValid = contractingPerformanceTaskListItemService.isValid(scapDetail);

    return new TaskListItem(
        CONTRACTING_PERFORMANCE_DISPLAY_NAME,
        TaskListLabel.from(isValid),
        actionUrl
    );
  }

  /* Project performance and close-out */
  TaskListItem getProjectPerformanceTaskListItem(ScapId scapId, ScapDetail scapDetail) {
    var actionUrl = ReverseRouter.route(on(ProjectPerformanceController.class).renderProjectPerformanceForm(scapId));
    var projectPerformanceOpt = projectPerformanceService.findByScapDetail(scapDetail);
    var isValid = projectPerformanceOpt
        .map(projectPerformanceFormService::getForm)
        .map(form -> projectPerformanceFormService.validate(form, getBindingResultForForm(form)))
        .map(bindingResult -> !bindingResult.hasErrors())
        .orElse(false);

    return new TaskListItem(
        PROJECT_PERFORMANCE_DISPLAY_NAME,
        TaskListLabel.from(isValid),
        actionUrl
    );
  }

  /* Related pathfinder projects */
  TaskListItem getPathfinderTaskListItem(ScapId scapId, ScapDetail scapDetail) {
    var actionUrl = ReverseRouter.route(on(PathfinderController.class).renderPathfinderProjectsForm(scapId));
    var pathfinderOverviewOptional = pathfinderService.findPathfinderProjectsOverview(scapDetail);
    var pathfinderProjects = pathfinderOverviewOptional
        .map(pathfinderService::findAllByPathfinderProjectsOverview)
        .orElse(Collections.emptySet());
    var form = pathfinderOverviewOptional
        .map(pathfinderProjectsOverview -> PathfinderForm.from(pathfinderProjectsOverview, pathfinderProjects))
        .orElse(new PathfinderForm());
    var isValid = !pathfinderFormValidator.validate(form).hasErrors();

    return new TaskListItem(
        PATHFINDER_DISPLAY_NAME,
        TaskListLabel.from(isValid),
        actionUrl
    );
  }
}
