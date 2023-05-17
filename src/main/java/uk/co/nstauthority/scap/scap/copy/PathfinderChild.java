package uk.co.nstauthority.scap.scap.copy;

import java.util.UUID;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderProjectsOverview;

public interface PathfinderChild {

  void setId(UUID id);

  void setPathfinderProjectsOverview(PathfinderProjectsOverview pathfinderProjectsOverview);

  UUID getId();

  PathfinderProjectsOverview getPathfinderProjectsOverview();
}
