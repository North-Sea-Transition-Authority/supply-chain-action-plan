package uk.co.nstauthority.scap.scap.projectdetails;

import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "project_details")
class ProjectDetails {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private String projectName;

  private BigDecimal projectCostEstimate;

  private BigDecimal estimatedValueLocalContent;

  private Integer fieldId;

  private String fieldName;

  private LocalDate plannedExecutionStartDate;

  private LocalDate plannedCompletionDate;

  private Boolean hasFacilities;
  private Instant createdTimestamp;

  public ProjectDetails() {

  }

  ProjectDetails(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  String getProjectName() {
    return projectName;
  }

  void setProjectName(String projectName) {
    this.projectName = projectName;
  }

  BigDecimal getProjectCostEstimate() {
    return projectCostEstimate;
  }

  void setProjectCostEstimate(BigDecimal projectCostEstimate) {
    this.projectCostEstimate = projectCostEstimate;
  }

  BigDecimal getEstimatedValueLocalContent() {
    return estimatedValueLocalContent;
  }

  void setEstimatedValueLocalContent(BigDecimal estimatedValueLocalContent) {
    this.estimatedValueLocalContent = estimatedValueLocalContent;
  }

  Integer getFieldId() {
    return fieldId;
  }

  void setFieldId(Integer fieldId) {
    this.fieldId = fieldId;
  }

  public String getFieldName() {
    return fieldName;
  }

  public void setFieldName(String fieldName) {
    this.fieldName = fieldName;
  }

  LocalDate getPlannedExecutionStartDate() {
    return plannedExecutionStartDate;
  }

  void setPlannedExecutionStartDate(LocalDate plannedExecutionStartDate) {
    this.plannedExecutionStartDate = plannedExecutionStartDate;
  }

  LocalDate getPlannedCompletionDate() {
    return plannedCompletionDate;
  }

  void setPlannedCompletionDate(LocalDate plannedCompletionDate) {
    this.plannedCompletionDate = plannedCompletionDate;
  }

  public Boolean getHasFacilities() {
    return hasFacilities;
  }

  public void setHasFacilities(Boolean hasFacilities) {
    this.hasFacilities = hasFacilities;
  }

  Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
