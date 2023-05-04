package uk.co.nstauthority.scap.scap.pathfinder;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.TestEntityProvider;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
class PathfinderServiceTest {

  @Mock
  private PathfinderProjectRepository pathfinderProjectRepository;

  @Mock
  private PathfinderProjectsOverviewRepository pathfinderProjectsOverviewRepository;

  @Mock
  private PathfinderProjectService pathfinderProjectService;

  @Mock
  private Clock clock;

  @InjectMocks
  private PathfinderService pathfinderService;

  @Captor
  private ArgumentCaptor<PathfinderProjectsOverview> overviewArgumentCaptor;

  @Captor
  private ArgumentCaptor<Set<PathfinderProject>> pathfinderProjectsArgumentCaptor;

  private static final ScapDetail SCAP_DETAIL = TestEntityProvider.getScapDetail();

  @Test
  void saveRelatedPathfinderProjects_NoExistingOverview() {
    var form = new PathfinderForm();
    form.setHasPathfinderProjects(false);
    form.setNoPathfinderProjectRationale("some rationale");

    when(pathfinderProjectsOverviewRepository.findByScapDetail(SCAP_DETAIL)).thenReturn(Optional.empty());

    pathfinderService.saveRelatedPathfinderProjects(SCAP_DETAIL, form);

    verify(pathfinderProjectsOverviewRepository).save(overviewArgumentCaptor.capture());

    assertThat(overviewArgumentCaptor.getValue()).extracting(
        PathfinderProjectsOverview::getScapDetail,
        PathfinderProjectsOverview::getHasRelatedPathfinderProjects,
        PathfinderProjectsOverview::getNoPathfinderProjectsRationale
    ).containsExactly(
        SCAP_DETAIL,
        form.getHasPathfinderProjects(),
        form.getNoPathfinderProjectRationale().getInputValue()
    );
  }

  @Test
  void saveRelatedPathfinderProjects_ExistingProjects() {
    var form = new PathfinderForm();
    var projectIds = Collections.singletonList(12);
    form.setHasPathfinderProjects(true);
    form.setPathfinderProjectIds(projectIds);
    var existingPathfinderProjectsOverview = new PathfinderProjectsOverview(SCAP_DETAIL, Instant.now());
    var existingProjects = Collections.singleton(new PathfinderProject());
    var pathfinderProject = uk.co.fivium.energyportalapi.generated.types.PathfinderProject
        .newBuilder()
        .projectId(12)
        .projectTitle("Some Pathfinder project title")
        .build();
    var createdInstant = Instant.now();

    when(pathfinderProjectsOverviewRepository.findByScapDetail(SCAP_DETAIL))
        .thenReturn(Optional.of(existingPathfinderProjectsOverview));
    when(pathfinderProjectRepository.findAllByPathfinderProjectsOverview(existingPathfinderProjectsOverview))
        .thenReturn(existingProjects);
    when(pathfinderProjectService.getPathfinderProjectsByIds(projectIds))
        .thenReturn(Collections.singletonList(pathfinderProject));
    when(clock.instant()).thenReturn(createdInstant);

    pathfinderService.saveRelatedPathfinderProjects(SCAP_DETAIL, form);

    verify(pathfinderProjectsOverviewRepository).save(existingPathfinderProjectsOverview);

    assertThat(existingPathfinderProjectsOverview).extracting(
        PathfinderProjectsOverview::getScapDetail,
        PathfinderProjectsOverview::getHasRelatedPathfinderProjects,
        PathfinderProjectsOverview::getNoPathfinderProjectsRationale
    ).containsExactly(
        SCAP_DETAIL,
        form.getHasPathfinderProjects(),
        form.getNoPathfinderProjectRationale().getInputValue()
    );

    verify(pathfinderProjectRepository).deleteAll(existingProjects);
    verify(pathfinderProjectRepository).saveAll(pathfinderProjectsArgumentCaptor.capture());

    assertThat(pathfinderProjectsArgumentCaptor.getValue()).extracting(
        PathfinderProject::getPathfinderProjectId,
        PathfinderProject::getPathfinderProjectName,
        PathfinderProject::getRelatedPathfinderProjectsOverview,
        PathfinderProject::getCreatedTimestamp
    ).containsExactly(
        tuple(
            pathfinderProject.getProjectId(),
            pathfinderProject.getProjectTitle(),
            existingPathfinderProjectsOverview,
            createdInstant
        )
    );
  }

  @Test
  void findPathfinderProjectsOverview() {
    var pathfinderProjectsOverview = new PathfinderProjectsOverview();

    when(pathfinderProjectsOverviewRepository.findByScapDetail(SCAP_DETAIL))
        .thenReturn(Optional.of(pathfinderProjectsOverview));

    var returnedPathfinderProjectsOverview = pathfinderService.findPathfinderProjectsOverview(SCAP_DETAIL);

    assertThat(returnedPathfinderProjectsOverview).contains(pathfinderProjectsOverview);
  }

  @Test
  void findAllByPathfinderProjectsOverview_HasNoRelatedProjects() {
    var pathfinderProjectsOverview = new PathfinderProjectsOverview();
    pathfinderProjectsOverview.setHasRelatedPathfinderProjects(false);

    var pathfinderProjects = pathfinderService.findAllByPathfinderProjectsOverview(pathfinderProjectsOverview);
    assertThat(pathfinderProjects).isEmpty();

    verifyNoInteractions(pathfinderProjectRepository, pathfinderProjectsOverviewRepository);
  }

  @Test
  void findAllByPathfinderProjectsOverview_HasRelatedProjects() {
    var pathfinderProjectsOverview = new PathfinderProjectsOverview();
    pathfinderProjectsOverview.setHasRelatedPathfinderProjects(true);
    var projects = Collections.singleton(
        new PathfinderProject(
            pathfinderProjectsOverview,
            347,
            "some Pathfinder title",
            Instant.now()
        )
    );

    when(pathfinderProjectRepository.findAllByPathfinderProjectsOverview(pathfinderProjectsOverview))
        .thenReturn(projects);

    var pathfinderProjects = pathfinderService.findAllByPathfinderProjectsOverview(pathfinderProjectsOverview);
    assertThat(pathfinderProjects).isEqualTo(projects);
  }
}
