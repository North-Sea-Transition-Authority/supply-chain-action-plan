package uk.co.nstauthority.scap.scap.pathfinder;

import com.google.common.annotations.VisibleForTesting;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;
import org.hibernate.annotations.UuidGenerator;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "pathfinder_projects_overviews")
public class PathfinderProjectsOverview {

  @Id
  @UuidGenerator
  private UUID id;

  @OneToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  private Boolean hasRelatedPathfinderProjects;

  private String noPathfinderProjectsRationale;

  private Instant createdTimestamp;

  public PathfinderProjectsOverview() {
  }

  @VisibleForTesting
  public PathfinderProjectsOverview(UUID id) {
    this.id = id;
  }

  public PathfinderProjectsOverview(ScapDetail scapDetail, Instant createdTimestamp) {
    this.scapDetail = scapDetail;
    this.createdTimestamp = createdTimestamp;
  }

  public void update(PathfinderForm form) {
    this.hasRelatedPathfinderProjects = form.getHasPathfinderProjects();
    if (Boolean.FALSE.equals(form.getHasPathfinderProjects())) {
      this.noPathfinderProjectsRationale = form.getNoPathfinderProjectRationale().getInputValue();
    } else {
      this.noPathfinderProjectsRationale = null;
    }
  }

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public ScapDetail getScapDetail() {
    return scapDetail;
  }

  public void setScapDetail(ScapDetail scapDetail) {
    this.scapDetail = scapDetail;
  }

  public Boolean getHasRelatedPathfinderProjects() {
    return hasRelatedPathfinderProjects;
  }

  public void setHasRelatedPathfinderProjects(Boolean hasRelatedPathfinderProjects) {
    this.hasRelatedPathfinderProjects = hasRelatedPathfinderProjects;
  }

  public String getNoPathfinderProjectsRationale() {
    return noPathfinderProjectsRationale;
  }

  public void setNoPathfinderProjectsRationale(String noPathfinderProjectsRationale) {
    this.noPathfinderProjectsRationale = noPathfinderProjectsRationale;
  }

  public void setCreatedTimestamp(Instant createdTimestamp) {
    this.createdTimestamp = createdTimestamp;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
