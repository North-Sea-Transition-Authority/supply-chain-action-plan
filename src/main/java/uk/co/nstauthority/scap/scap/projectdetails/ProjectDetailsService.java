package uk.co.nstauthority.scap.scap.projectdetails;

import java.time.Clock;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.energyportal.FieldService;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
@Transactional
public class ProjectDetailsService {

  private final ProjectDetailsRepository projectDetailsRepository;
  private final ProjectDetailTypeRepository projectDetailTypeRepository;
  private final ProjectFacilityRepository projectFacilityRepository;
  private final Clock clock;
  private final FieldService fieldService;
  private final FileUploadService fileUploadService;

  @Autowired
  ProjectDetailsService(ProjectDetailsRepository projectDetailsRepository,
                        ProjectDetailTypeRepository projectDetailTypeRepository,
                        ProjectFacilityRepository projectFacilityRepository, Clock clock, FieldService fieldService,
                        FileUploadService fileUploadService) {
    this.projectDetailsRepository = projectDetailsRepository;
    this.projectDetailTypeRepository = projectDetailTypeRepository;
    this.projectFacilityRepository = projectFacilityRepository;
    this.clock = clock;
    this.fieldService = fieldService;
    this.fileUploadService = fileUploadService;
  }

  Set<ProjectType> getProjectTypesByProjectDetails(ProjectDetails projectDetails) {
    return projectDetailTypeRepository.findAllByProjectDetails(projectDetails).stream()
        .map(ProjectDetailType::getProjectType)
        .collect(Collectors.toSet());
  }

  Optional<ProjectDetails> getProjectDetailsByScapDetail(ScapDetail scapDetail) {
    return projectDetailsRepository.findByScapDetail(scapDetail);
  }

  @Transactional
  void saveProjectDetails(ScapDetail scapDetail, ProjectDetailsForm form) {
    var createdTimestamp = clock.instant();
    var projectDetails = projectDetailsRepository.findByScapDetail(scapDetail)
        .orElse(new ProjectDetails(scapDetail, createdTimestamp));

    updateProjectDetails(projectDetails, form);
    updateProjectDetailTypes(projectDetails, form.getProjectTypes(), createdTimestamp);
    fileUploadService.updateFileUploadDescriptions(form.getSupportingDocuments());

    if (YesNo.YES.equals(form.getHasPlatforms())) {
      saveProjectFacilities(projectDetails, form.getInstallationIds(), createdTimestamp);
    } else {
      saveProjectFacilities(projectDetails, Collections.emptyList(), createdTimestamp);
    }
  }

  List<ProjectFacility> getProjectFacilities(ProjectDetails projectDetails) {
    return projectFacilityRepository.findAllByProjectDetails(projectDetails);
  }

  private void saveProjectFacilities(ProjectDetails projectDetails, List<Integer> facilityIds, Instant createdTimestamp) {
    var existingProjectFacilities = getProjectFacilities(projectDetails);
    var existingProjectFacilityIds = existingProjectFacilities
        .stream()
        .map(ProjectFacility::getFacilityId)
        .collect(Collectors.toSet());

    var addedFacilityIds = facilityIds.stream()
        .filter(facilityId -> !existingProjectFacilityIds.contains(facilityId))
        .collect(Collectors.toSet());

    var removedFacilityIds = existingProjectFacilityIds.stream()
        .filter(existingFacilityId -> !facilityIds.contains(existingFacilityId))
        .collect(Collectors.toSet());

    var newProjectFacilities = addedFacilityIds.stream()
        .map(facilityId -> new ProjectFacility(projectDetails, createdTimestamp, facilityId))
        .collect(Collectors.toSet());

    var removedProjectFacilities = existingProjectFacilities.stream()
        .filter(existingFacility -> removedFacilityIds.contains(existingFacility.getFacilityId()))
        .collect(Collectors.toSet());

    projectFacilityRepository.deleteAll(removedProjectFacilities);
    projectFacilityRepository.saveAll(newProjectFacilities);

  }

  private void saveProjectDetailType(ProjectDetails projectDetails, ProjectType projectType, Instant createdTimestamp) {
    var projectDetailType = new ProjectDetailType(projectDetails, createdTimestamp);
    projectDetailType.setProjectType(projectType);
    projectDetailTypeRepository.save(projectDetailType);
  }

  private void updateProjectDetailTypes(ProjectDetails projectDetails, Set<ProjectType> projectTypes, Instant createdTimestamp) {
    var existingProjectDetailTypes = projectDetailTypeRepository.findAllByProjectDetails(projectDetails);
    var existingProjectTypes = existingProjectDetailTypes.stream()
        .map(ProjectDetailType::getProjectType)
        .collect(Collectors.toSet());

    existingProjectDetailTypes.stream()
        .filter(existingProjectDetailType -> !projectTypes.contains(existingProjectDetailType.getProjectType()))
        .forEach(projectDetailTypeRepository::delete);

    projectTypes.stream()
        .filter(projectType -> !existingProjectTypes.contains(projectType))
        .forEach(projectType -> saveProjectDetailType(projectDetails, projectType, createdTimestamp));
  }

  private void updateProjectDetails(ProjectDetails projectDetails, ProjectDetailsForm form) {
    projectDetails.setProjectName(form.getProjectName().getInputValue());
    projectDetails.setProjectCostEstimate(form.getProjectCostEstimate().getAsBigDecimal().orElse(null));
    projectDetails.setEstimatedValueLocalContent(form.getEstimatedValueLocalContent().getAsBigDecimal().orElse(null));
    form.getFieldId().getAsInteger().ifPresent(fieldId -> {
      var field = fieldService.getFieldById(fieldId, "Get field name to save SCAP project details");
      field.ifPresent(actualField -> {
        projectDetails.setFieldId(fieldId);
        projectDetails.setFieldName(actualField.getFieldName());
      });
    });
    var startDate = form.getExpectedStartDate()
        .getAsLocalDate().orElse(null);
    var endDate = form.getExpectedEndDate()
        .getAsLocalDate().orElse(null);
    projectDetails.setHasFacilities(YesNo.YES.equals(form.getHasPlatforms()));
    projectDetails.setPlannedExecutionStartDate(startDate);
    projectDetails.setPlannedCompletionDate(endDate);
    projectDetailsRepository.save(projectDetails);
  }
}
