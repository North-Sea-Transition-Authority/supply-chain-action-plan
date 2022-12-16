package uk.co.nstauthority.scap.scap.summary;

import com.google.common.annotations.VisibleForTesting;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.ModelAndView;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;

@Service
public class ScapSummaryViewService {

  private final ProjectDetailsService projectDetailsService;

  @Autowired
  ScapSummaryViewService(ProjectDetailsService projectDetailsService) {
    this.projectDetailsService = projectDetailsService;
  }

  @Transactional
  public ModelAndView addScapSummaryToModel(ModelAndView modelAndView, ScapDetail scapDetail) {
    modelAndView.addObject("projectDetailsSummaryView", getProjectDetailsSummaryView(scapDetail));
    return modelAndView;
  }

  @VisibleForTesting
  public ProjectDetailsSummaryView getProjectDetailsSummaryView(ScapDetail scapDetail) {
    var projectDetails = projectDetailsService.getProjectDetailsOrThrow(scapDetail);
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
  }
}
