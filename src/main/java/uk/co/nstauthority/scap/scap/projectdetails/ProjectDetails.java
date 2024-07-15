package uk.co.nstauthority.scap.scap.projectdetails;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import org.hibernate.annotations.CreationTimestamp;
import uk.co.nstauthority.scap.scap.copy.ScapDetailChild;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "project_details")
public class ProjectDetails implements ScapDetailChild {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private String projectName;

  private String projectSummary;

  private BigDecimal projectCostEstimate;

  private Boolean awareOfLocalContentCommitment;

  private Boolean expectsToMeetLocalContentCommitment;

  private String missLocalContentCommitmentRationale;

  private LocalDate plannedExecutionStartDate;

  private LocalDate plannedCompletionDate;

  private Boolean hasFacilities;

  @CreationTimestamp
  private Instant createdTimestamp;

  public ProjectDetails() {

  }

  public ProjectDetails(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  @Override
  public Integer getId() {
    return id;
  }

  @Override
  public void setId(Integer id) {
    this.id = id;
  }

  @Override
  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  @Override
  public void setScapDetail(ScapDetail scapDetail) {
    this.scapDetail = scapDetail;
  }

  public String getProjectName() {
    return projectName;
  }

  public void setProjectName(String projectName) {
    this.projectName = projectName;
  }

  public String getProjectSummary() {
    return projectSummary;
  }

  public void setProjectSummary(String projectSummary) {
    this.projectSummary = projectSummary;
  }

  public BigDecimal getProjectCostEstimate() {
    return projectCostEstimate;
  }

  public void setProjectCostEstimate(BigDecimal projectCostEstimate) {
    this.projectCostEstimate = projectCostEstimate;
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

  public String getMissLocalContentCommitmentRationale() {
    return missLocalContentCommitmentRationale;
  }

  public void setMissLocalContentCommitmentRationale(String missLocalContentCommitmentRationale) {
    this.missLocalContentCommitmentRationale = missLocalContentCommitmentRationale;
  }

  public LocalDate getPlannedExecutionStartDate() {
    return plannedExecutionStartDate;
  }

  public void setPlannedExecutionStartDate(LocalDate plannedExecutionStartDate) {
    this.plannedExecutionStartDate = plannedExecutionStartDate;
  }

  public LocalDate getPlannedCompletionDate() {
    return plannedCompletionDate;
  }

  public void setPlannedCompletionDate(LocalDate plannedCompletionDate) {
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
