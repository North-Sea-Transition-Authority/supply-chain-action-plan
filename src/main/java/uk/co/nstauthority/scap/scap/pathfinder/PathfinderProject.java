package uk.co.nstauthority.scap.scap.pathfinder;

import com.google.common.annotations.VisibleForTesting;
import java.time.Instant;
import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "pathfinder_projects")
public class PathfinderProject {

  @Id
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

  public UUID getId() {
    return id;
  }

  public String getPathfinderProjectName() {
    return pathfinderProjectName;
  }

  public PathfinderProjectsOverview getRelatedPathfinderProjectsOverview() {
    return pathfinderProjectsOverview;
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
