package uk.co.nstauthority.scap.energyportal;

import java.util.Collections;
import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.pathfinder.PathfinderApi;
import uk.co.fivium.energyportalapi.generated.client.PathfinderProjectsProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.PathfinderProject;
import uk.co.fivium.energyportalapi.generated.types.PathfinderProjectType;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;

@Service
public class PathfinderProjectService {

  private final PathfinderApi pathfinderApi;

  static final PathfinderProjectsProjectionRoot PATHFINDER_PROJECTS_PROJECTION_ROOT =
      new PathfinderProjectsProjectionRoot()
          .projectId()
          .projectTitle();
  static final RequestPurpose FIND_ALL_BY_IDS_REQUEST_PURPOSE =
      new RequestPurpose("Pre-fill Related Pathfinder projects form for SCAP");

  @Autowired
  public PathfinderProjectService(PathfinderApi pathfinderApi) {
    this.pathfinderApi = pathfinderApi;
  }

  public RestSearchResult searchProjects(String term, String searchPurpose, Integer operatorOrganisationGroupId) {
    var results = pathfinderApi.searchProjects(
        null,
        null,
        term,
        operatorOrganisationGroupId,
        PathfinderProjectType.INFRASTRUCTURE,
        PATHFINDER_PROJECTS_PROJECTION_ROOT,
        new RequestPurpose(searchPurpose)
    );
    var restSearchItems = results.stream()
        .map(pathfinderProject -> new RestSearchItem(
            pathfinderProject.getProjectId().toString(),
            pathfinderProject.getProjectTitle()
        ))
        .toList();
    return new RestSearchResult(restSearchItems);
  }

  public List<PathfinderProject> getPathfinderProjectsByIds(List<Integer> pathfinderProjectIds) {
    if (pathfinderProjectIds.isEmpty()) {
      return Collections.emptyList();
    }

    return pathfinderApi.getProjectsByIds(
        pathfinderProjectIds,
        PATHFINDER_PROJECTS_PROJECTION_ROOT,
        FIND_ALL_BY_IDS_REQUEST_PURPOSE
    );
  }

  public List<AddToListItem> getProjectAddToListItems(List<Integer> pathfinderProjectIds) {
    var results = getPathfinderProjectsByIds(pathfinderProjectIds);
    return results.stream()
        .map(pathfinderProject -> new AddToListItem(
            pathfinderProject.getProjectId().toString(),
            pathfinderProject.getProjectTitle(),
            true
        )).toList();
  }
}
