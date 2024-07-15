package uk.co.nstauthority.scap.scap.projectperformance;

import com.google.common.annotations.VisibleForTesting;
import jakarta.persistence.Column;
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
@Table(name = "project_performances")
public class ProjectPerformance implements ScapDetailChild {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Integer id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  @Column(name = "is_project_completed")
  private Boolean projectCompleted;

  private LocalDate startDate;

  private LocalDate completionDate;

  private BigDecimal outturnCost;

  @CreationTimestamp
  private Instant createdTimestamp;

  public ProjectPerformance() {
  }

  @VisibleForTesting
  ProjectPerformance(Integer id) {
    this.id = id;
  }

  public ProjectPerformance(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  @Override
  public void setId(Integer id) {
    this.id = id;
  }

  @Override
  public void setScapDetail(ScapDetail scapDetail) {
    this.scapDetail = scapDetail;
  }

  public Integer getId() {
    return id;
  }

  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  public Boolean getProjectCompleted() {
    return projectCompleted;
  }

  public void setProjectCompleted(Boolean projectCompleted) {
    this.projectCompleted = projectCompleted;
  }

  public LocalDate getStartDate() {
    return startDate;
  }

  public void setStartDate(LocalDate startDate) {
    this.startDate = startDate;
  }

  public LocalDate getCompletionDate() {
    return completionDate;
  }

  public void setCompletionDate(LocalDate completionDate) {
    this.completionDate = completionDate;
  }

  public BigDecimal getOutturnCost() {
    return outturnCost;
  }

  public void setOutturnCost(BigDecimal outturnCost) {
    this.outturnCost = outturnCost;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
