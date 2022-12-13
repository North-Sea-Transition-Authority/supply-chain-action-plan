package uk.co.nstauthority.scap.scap.projectdetails;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.IntegerInput;
import uk.co.fivium.formlibrary.input.StringInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.file.FileUploadForm;

class ProjectDetailsForm {

  private final StringInput projectName;
  private Set<ProjectType> projectTypes;
  private final DecimalInput projectCostEstimate;
  private final DecimalInput estimatedValueLocalContent;
  private final IntegerInput fieldId;
  private YesNo hasPlatforms;
  private List<Integer> installationIds;
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
    this.projectName = new StringInput("projectName", "Project name");
    this.projectCostEstimate = new DecimalInput("projectCostEstimate", "Project cost estimate");
    this.estimatedValueLocalContent = new DecimalInput("estimatedValueLocalContent",
        "Estimated value local content");
    this.fieldId = new IntegerInput("fieldId", "Field");
    this.startDay = new IntegerInput("startDay", "Start day");
    this.startMonth = new IntegerInput("startMonth", "Start month");
    this.startYear = new IntegerInput("startYear", "Start year");
    this.endDay = new IntegerInput("endDay", "End day");
    this.endMonth = new IntegerInput("endMonth", "End month");
    this.endYear = new IntegerInput("endYear", "End year");
    this.expectedStartDate = new ThreeFieldDateInput(
        "startDate",
        "Indicative planned execution start date",
        this.startDay,
        this.startMonth,
        this.startYear
    );
    this.expectedEndDate = new ThreeFieldDateInput(
        "endDate",
        "Indicative planned execution end date",
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

  public IntegerInput getFieldId() {
    return fieldId;
  }

  public void setFieldId(String fieldId) {
    this.fieldId.setInputValue(fieldId);
  }

  public YesNo getHasPlatforms() {
    return hasPlatforms;
  }

  public void setHasPlatforms(YesNo hasPlatforms) {
    this.hasPlatforms = hasPlatforms;
  }

  public List<Integer> getInstallationIds() {
    return installationIds;
  }

  public void setInstallationIds(List<Integer> installationIds) {
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
