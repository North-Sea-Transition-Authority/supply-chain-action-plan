package uk.co.nstauthority.scap.scap.summary;

import com.google.common.annotations.VisibleForTesting;
import java.util.Collections;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.ModelAndView;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivityService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;

@Service
public class ScapSummaryViewService {

  public static final String  PROJECT_DETAILS_OBJECT_NAME = "projectDetailsSummaryView";
  public static final String PLANNED_TENDER_OBJECT_NAME = "plannedTenderSummaryView";

  private final ProjectDetailsService projectDetailsService;
  private final PlannedTenderService plannedTenderService;
  private final PlannedTenderActivityService plannedTenderActivityService;

  @Autowired
  ScapSummaryViewService(ProjectDetailsService projectDetailsService,
                         PlannedTenderService plannedTenderService,
                         PlannedTenderActivityService plannedTenderActivityService) {
    this.projectDetailsService = projectDetailsService;
    this.plannedTenderService = plannedTenderService;
    this.plannedTenderActivityService = plannedTenderActivityService;
  }

  @Transactional
  public ModelAndView addScapSummaryToModel(ModelAndView modelAndView, ScapDetail scapDetail) {
    modelAndView.addObject(PROJECT_DETAILS_OBJECT_NAME, getProjectDetailsSummaryView(scapDetail));
    modelAndView.addObject(PLANNED_TENDER_OBJECT_NAME, getPlannedTenderSummaryView(scapDetail));
    return modelAndView;
  }

  @VisibleForTesting
  public ProjectDetailsSummaryView getProjectDetailsSummaryView(ScapDetail scapDetail) {
    var projectDetailsOpt = projectDetailsService.getProjectDetails(scapDetail);
    return projectDetailsOpt.map(projectDetails -> {
      var projectTypes = projectDetailsService.getProjectTypesByProjectDetails(projectDetails)
          .stream().toList();
      var projectFacilities = projectDetailsService.getProjectFacilityNames(projectDetails);
      var hasFacilities = YesNo.fromBoolean(projectDetails.getHasFacilities());

      return new ProjectDetailsSummaryView(
          projectDetails.getProjectName(),
          projectTypes,
          projectDetails.getProjectCostEstimate(),
          projectDetails.getEstimatedValueLocalContent(),
          projectDetails.getFieldName(),
          hasFacilities,
          projectFacilities,
          DateUtils.format(projectDetails.getPlannedExecutionStartDate()),
          DateUtils.format(projectDetails.getPlannedCompletionDate())
      );
    }).orElse(new ProjectDetailsSummaryView(
        null, Collections.emptyList(), null, null, null,
        null, Collections.emptyList(), null, null
    ));
  }

  @VisibleForTesting
  public PlannedTenderSummaryView getPlannedTenderSummaryView(ScapDetail scapDetail) {
    var plannedTenderOpt = plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail);
    return plannedTenderOpt.map(
        plannedTender -> {
          if (Boolean.FALSE.equals(plannedTender.getHasPlannedTenders())) {
            return new PlannedTenderSummaryView(false, Collections.emptyList());
          }

          var plannedTenderActivities = plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender);
          var plannedTenderActivitySummaryViews = plannedTenderActivities.stream()
              .map(PlannedTenderActivitySummaryView::from)
              .toList();
          return new PlannedTenderSummaryView(plannedTender.getHasPlannedTenders(), plannedTenderActivitySummaryViews);
        }
    ).orElse(new PlannedTenderSummaryView(null, null));


  }
}
