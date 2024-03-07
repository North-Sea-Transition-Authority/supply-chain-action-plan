package uk.co.nstauthority.scap.scap.projectdetails;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;
import uk.co.nstauthority.scap.file.FileUploadForm;

public class ProjectDetailsForm {

  private final StringInput projectName;
  private final StringInput projectSummary;
  private Set<ProjectType> projectTypes;
  private final DecimalInput projectCostEstimate;
  private Boolean awareOfLocalContentCommitment;
  private Boolean expectsToMeetLocalContentCommitment;
  private final StringInput willMissLocalContentCommitmentRationale;
  private Integer fieldSelector;
  private Set<Integer> fieldIds;
  private Boolean hasPlatforms;
  private Set<Integer> installationIds;
  private Integer installationSelector;
  private final ThreeFieldDateInput expectedStartDate;
  private final ThreeFieldDateInput expectedEndDate;
  private List<FileUploadForm> supportingDocuments = new ArrayList<>();

  public ProjectDetailsForm() {
    this.projectName = new StringInput("projectName", "the project name");
    this.projectSummary = new StringInput("projectSummary", "the project summary");
    this.projectCostEstimate = new DecimalInput("projectCostEstimate", "the project cost estimate");
    this.willMissLocalContentCommitmentRationale = new StringInput(
        "willMissLocalContentCommitmentRationale",
        "rationale on why you do not expect the target will be reached"
    );
    this.expectedStartDate = new ThreeFieldDateInput(
        "expectedStartDate",
        "indicative planned execution start date"
    );
    this.expectedEndDate = new ThreeFieldDateInput(
        "expectedEndDate",
        "indicative planned execution end date"
    );
  }

  public StringInput getProjectName() {
    return projectName;
  }

  public void setProjectName(String projectName) {
    this.projectName.setInputValue(projectName);
  }

  public StringInput getProjectSummary() {
    return projectSummary;
  }

  public void setProjectSummary(String projectSummary) {
    this.projectSummary.setInputValue(projectSummary);
  }

  public Set<ProjectType> getProjectTypes() {
    return projectTypes;
  }

  public void setProjectTypes(Set<ProjectType> projectTypes) {
    this.projectTypes = projectTypes;
  }

  public DecimalInput getProjectCostEstimate() {
    return projectCostEstimate;
  }

  public void setProjectCostEstimate(String projectCostEstimate) {
    this.projectCostEstimate.setInputValue(projectCostEstimate);
  }

  public Boolean getAwareOfLocalContentCommitment() {
    return awareOfLocalContentCommitment;
  }

  public void setAwareOfLocalContentCommitment(Boolean awareOfLocalContentCommitment) {
    this.awareOfLocalContentCommitment = awareOfLocalContentCommitment;
  }

  public Boolean getExpectsToMeetLocalContentCommitment() {
    return expectsToMeetLocalContentCommitment;
  }

  public void setExpectsToMeetLocalContentCommitment(Boolean expectsToMeetLocalContentCommitment) {
    this.expectsToMeetLocalContentCommitment = expectsToMeetLocalContentCommitment;
  }

  public StringInput getWillMissLocalContentCommitmentRationale() {
    return willMissLocalContentCommitmentRationale;
  }

  public void setWillMissLocalContentCommitmentRationale(String willMissLocalContentCommitmentRationale) {
    this.willMissLocalContentCommitmentRationale.setInputValue(willMissLocalContentCommitmentRationale);
  }

  public Set<Integer> getFieldIds() {
    return fieldIds;
  }

  public void setFieldIds(Set<Integer> fieldIds) {
    this.fieldIds = fieldIds;
  }

  public Integer getFieldSelector() {
    return fieldSelector;
  }

  public void setFieldSelector(Integer fieldSelector) {
    this.fieldSelector = fieldSelector;
  }

  public Boolean getHasPlatforms() {
    return hasPlatforms;
  }

  public void setHasPlatforms(Boolean hasPlatforms) {
    this.hasPlatforms = hasPlatforms;
  }

  public Set<Integer> getInstallationIds() {
    return installationIds;
  }

  public void setInstallationIds(Set<Integer> installationIds) {
    this.installationIds = installationIds;
  }

  public Integer getInstallationSelector() {
    return installationSelector;
  }

  public void setInstallationSelector(Integer installationSelector) {
    this.installationSelector = installationSelector;
  }

  public ThreeFieldDateInput getExpectedStartDate() {
    return expectedStartDate;
  }

  public void setExpectedStartDate(LocalDate localDate) {
    expectedStartDate.setDate(localDate);
  }

  public ThreeFieldDateInput getExpectedEndDate() {
    return expectedEndDate;
  }

  public void setExpectedEndDate(LocalDate localDate) {
    expectedEndDate.setDate(localDate);
  }

  public List<FileUploadForm> getSupportingDocuments() {
    return supportingDocuments;
  }

  public void setSupportingDocuments(List<FileUploadForm> supportingDocuments) {
    this.supportingDocuments = supportingDocuments;
  }
}
