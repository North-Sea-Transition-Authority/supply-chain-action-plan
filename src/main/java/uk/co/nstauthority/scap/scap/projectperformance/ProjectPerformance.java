package uk.co.nstauthority.scap.scap.projectperformance;

import com.google.common.annotations.VisibleForTesting;
import java.math.BigDecimal;
import java.time.Instant;
import java.time.LocalDate;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
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
