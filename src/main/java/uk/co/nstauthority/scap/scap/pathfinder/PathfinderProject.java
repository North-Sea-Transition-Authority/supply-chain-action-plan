package uk.co.nstauthority.scap.scap.pathfinder;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import uk.co.nstauthority.scap.scap.copy.PathfinderChild;

@Entity
@Table(name = "pathfinder_projects")
public class PathfinderProject implements PathfinderChild {

  @Id
  @GeneratedValue(generator = "uuid")
  private UUID id;

  @ManyToOne
  @JoinColumn(name = "pathfinder_projects_overview_id")
  private PathfinderProjectsOverview pathfinderProjectsOverview;

  private Integer pathfinderProjectId;

  private String pathfinderProjectName;

  private Instant createdTimestamp;

  public PathfinderProject() {
  }

  @VisibleForTesting
  public PathfinderProject(UUID id) {
    this.id = id;
  }

  public PathfinderProject(PathfinderProjectsOverview pathfinderProjectsOverview,
                           Integer pathfinderProjectId,
                           String pathfinderProjectName,
                           Instant createdTimestamp) {
    this(UUID.randomUUID());
    this.pathfinderProjectsOverview = pathfinderProjectsOverview;
    this.pathfinderProjectId = pathfinderProjectId;
    this.pathfinderProjectName = pathfinderProjectName;
    this.createdTimestamp = createdTimestamp;
  }

  @Override
  public UUID getId() {
    return id;
  }

  @Override
  public void setId(UUID id) {
    this.id = id;
  }

  @Override
  public PathfinderProjectsOverview getPathfinderProjectsOverview() {
    return pathfinderProjectsOverview;
  }

  @Override
  public void setPathfinderProjectsOverview(
      PathfinderProjectsOverview pathfinderProjectsOverview) {
    this.pathfinderProjectsOverview = pathfinderProjectsOverview;
  }

  public String getPathfinderProjectName() {
    return pathfinderProjectName;
  }

  public Integer getPathfinderProjectId() {
    return pathfinderProjectId;
  }

  public void setPathfinderProjectName(String pathfinderProjectName) {
    this.pathfinderProjectName = pathfinderProjectName;
  }

  public Instant getCreatedTimestamp() {
    return createdTimestamp;
  }
}
