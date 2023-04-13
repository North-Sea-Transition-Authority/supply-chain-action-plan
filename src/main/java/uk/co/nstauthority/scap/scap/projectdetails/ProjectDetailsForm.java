package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.IntegerInput;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;
import uk.co.nstauthority.scap.file.FileUploadForm;

public class ProjectDetailsForm {

  private final StringInput projectName;
  private final StringInput projectSummary;
  private Set<ProjectType> projectTypes;
  private final DecimalInput projectCostEstimate;
  private final DecimalInput estimatedValueLocalContent;
  private Integer fieldSelector;
  private Set<Integer> fieldIds;
  private Boolean hasPlatforms;
  private Set<Integer> installationIds;
  private Integer installationSelector;
  private final IntegerInput startDay;
  private final IntegerInput startMonth;
  private final IntegerInput startYear;
  private final IntegerInput endDay;
  private final IntegerInput endMonth;
  private final IntegerInput endYear;
  private final ThreeFieldDateInput expectedStartDate;
  private final ThreeFieldDateInput expectedEndDate;
  private List<FileUploadForm> supportingDocuments = new ArrayList<>();

  public ProjectDetailsForm() {
    this.projectName = new StringInput("projectName", "the project name");
    this.projectSummary = new StringInput("projectSummary", "the project summary");
    this.projectCostEstimate = new DecimalInput("projectCostEstimate", "the project cost estimate");
    this.estimatedValueLocalContent = new DecimalInput(
        "estimatedValueLocalContent", "the estimated value of local content");
    // TODO SCAP2022-255: Remove deprecated DFL method
    this.startDay = new IntegerInput("startDay", "start day");
    this.startMonth = new IntegerInput("startMonth", "start month");
    this.startYear = new IntegerInput("startYear", "start year");
    this.endDay = new IntegerInput("endDay", "snd day");
    this.endMonth = new IntegerInput("endMonth", "end month");
    this.endYear = new IntegerInput("endYear", "end year");
    this.expectedStartDate = new ThreeFieldDateInput(
        "startDate",
        "indicative planned execution start date",
        this.startDay,
        this.startMonth,
        this.startYear
    );
    this.expectedEndDate = new ThreeFieldDateInput(
        "endDate",
        "indicative planned execution end date",
        this.endDay,
        this.endMonth,
        this.endYear
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

  public DecimalInput getEstimatedValueLocalContent() {
    return estimatedValueLocalContent;
  }

  public void setEstimatedValueLocalContent(String estimatedValueLocalContent) {
    this.estimatedValueLocalContent.setInputValue(estimatedValueLocalContent);
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

  public ThreeFieldDateInput getExpectedEndDate() {
    return expectedEndDate;
  }

  public IntegerInput getStartDay() {
    return startDay;
  }

  public IntegerInput getStartMonth() {
    return startMonth;
  }

  public IntegerInput getStartYear() {
    return startYear;
  }

  public IntegerInput getEndDay() {
    return endDay;
  }

  public IntegerInput getEndMonth() {
    return endMonth;
  }

  public IntegerInput getEndYear() {
    return endYear;
  }

  public void setStartDay(String startDay) {
    this.startDay.setInputValue(startDay);
  }

  public void setStartMonth(String startMonth) {
    this.startMonth.setInputValue(startMonth);
  }

  public void setStartYear(String startYear) {
    this.startYear.setInputValue(startYear);
  }

  public void setEndDay(String endDay) {
    this.endDay.setInputValue(endDay);
  }

  public void setEndMonth(String endMonth) {
    this.endMonth.setInputValue(endMonth);
  }

  public void setEndYear(String endYear) {
    this.endYear.setInputValue(endYear);
  }

  public List<FileUploadForm> getSupportingDocuments() {
    return supportingDocuments;
  }

  public void setSupportingDocuments(List<FileUploadForm> supportingDocuments) {
    this.supportingDocuments = supportingDocuments;
  }
}
