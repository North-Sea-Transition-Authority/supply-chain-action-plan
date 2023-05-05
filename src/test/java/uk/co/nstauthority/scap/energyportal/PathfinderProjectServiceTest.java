package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.pathfinder.PathfinderApi;
import uk.co.fivium.energyportalapi.generated.types.PathfinderProject;
import uk.co.fivium.energyportalapi.generated.types.PathfinderProjectType;
import uk.co.nstauthority.scap.fds.addtolist.AddToListItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;

@ExtendWith(MockitoExtension.class)
class PathfinderProjectServiceTest {

  @Mock
  private PathfinderApi pathfinderApi;

  @InjectMocks
  private PathfinderProjectService pathfinderProjectService;

  private static final PathfinderProject PATHFINDER_PROJECT = PathfinderProject.newBuilder()
      .projectTitle("my pathfinder project title")
      .projectId(743)
      .build();

  @Test
  void searchProjects() {
    var searchTerm = "my";
    var searchPurpose = "test request purpose";
    var operatorId = 55;
    var requestPurposeCaptor = ArgumentCaptor.forClass(RequestPurpose.class);

    doReturn(Collections.singletonList(PATHFINDER_PROJECT))
        .when(pathfinderApi)
        .searchProjects(
            eq(null),
            eq(null),
            eq(searchTerm),
            eq(operatorId),
            eq(PathfinderProjectType.INFRASTRUCTURE),
            eq(PathfinderProjectService.PATHFINDER_PROJECTS_PROJECTION_ROOT),
            requestPurposeCaptor.capture()
        );

    var restSearchResult = pathfinderProjectService.searchProjects(searchTerm, searchPurpose, operatorId);

    verify(pathfinderApi).searchProjects(
        eq(null),
        eq(null),
        eq(searchTerm),
        eq(operatorId),
        eq(PathfinderProjectType.INFRASTRUCTURE),
        eq(PathfinderProjectService.PATHFINDER_PROJECTS_PROJECTION_ROOT),
        requestPurposeCaptor.capture()
    );

    assertThat(requestPurposeCaptor.getValue()).extracting(RequestPurpose::purpose).isEqualTo(searchPurpose);
    assertThat(restSearchResult.getResults()).extracting(
        RestSearchItem::id,
        RestSearchItem::text
    ).containsExactly(
        tuple(
            PATHFINDER_PROJECT.getProjectId().toString(),
            PATHFINDER_PROJECT.getProjectTitle()
        )
    );
  }

  @Test
  void getPathfinderProjectsByIds() {
    var requestedProjectIds = Collections.singletonList(PATHFINDER_PROJECT.getProjectId());

    doReturn(Collections.singletonList(PATHFINDER_PROJECT))
        .when(pathfinderApi)
        .getProjectsByIds(
            requestedProjectIds,
            PathfinderProjectService.PATHFINDER_PROJECTS_PROJECTION_ROOT,
            PathfinderProjectService.FIND_ALL_BY_IDS_REQUEST_PURPOSE
        );

    var returnedProjects = pathfinderProjectService.getPathfinderProjectsByIds(requestedProjectIds);

    assertThat(returnedProjects).containsExactly(PATHFINDER_PROJECT);
  }

  @Test
  void getPathfinderProjectsByIds_EmptyList() {
    var returnedProjects = pathfinderProjectService.getPathfinderProjectsByIds(Collections.emptyList());

    assertThat(returnedProjects).isEmpty();
  }

  @Test
  void getProjectAddToListItems() {
    var requestedProjectIds = Collections.singletonList(PATHFINDER_PROJECT.getProjectId());

    doReturn(Collections.singletonList(PATHFINDER_PROJECT))
        .when(pathfinderApi)
        .getProjectsByIds(
            requestedProjectIds,
            PathfinderProjectService.PATHFINDER_PROJECTS_PROJECTION_ROOT,
            PathfinderProjectService.FIND_ALL_BY_IDS_REQUEST_PURPOSE
        );

    var returnedAddToListItems = pathfinderProjectService.getProjectAddToListItems(requestedProjectIds);

    assertThat(returnedAddToListItems).extracting(
        AddToListItem::getId,
        AddToListItem::getName,
        AddToListItem::isValid
    ).containsExactly(
        tuple(
            PATHFINDER_PROJECT.getProjectId().toString(),
            PATHFINDER_PROJECT.getProjectTitle(),
            true
        )
    );
  }
}
