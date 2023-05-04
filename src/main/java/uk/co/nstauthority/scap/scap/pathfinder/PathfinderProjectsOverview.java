package uk.co.nstauthority.scap.scap.pathfinder;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "pathfinder_projects_overviews")
public class PathfinderProjectsOverview {

  @Id
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
    this(UUID.randomUUID());
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

  public ScapDetail getScapDetail() {
    return scapDetail;
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

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
