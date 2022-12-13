package uk.co.nstauthority.scap.scap.submit;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.scap.submit.submissionviews.ProjectDetailsSubmissionView;

@Service
class SubmissionViewService {

  private final ProjectDetailsService projectDetailsService;

  @Autowired
  SubmissionViewService(ProjectDetailsService projectDetailsService) {
    this.projectDetailsService = projectDetailsService;
  }

  ProjectDetailsSubmissionView getProjectDetailsSubmissionView(ScapDetail scapDetail) {
    var projectDetails = projectDetailsService.getProjectDetailsOrThrow(scapDetail);
    var projectTypes = projectDetailsService.getProjectTypesByProjectDetails(projectDetails)
        .stream().toList();
    var projectFacilities = projectDetailsService.getProjectFacilityNames(projectDetails);
    var hasFacilities = YesNo.fromBoolean(projectDetails.getHasFacilities());

    return new ProjectDetailsSubmissionView(
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
