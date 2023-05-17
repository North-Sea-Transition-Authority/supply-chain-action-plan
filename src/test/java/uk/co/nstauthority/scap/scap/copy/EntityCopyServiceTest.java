package uk.co.nstauthority.scap.scap.copy;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Stream;
import javax.persistence.EntityManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderProject;
import uk.co.nstauthority.scap.scap.pathfinder.PathfinderProjectsOverview;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailType;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectFacility;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectField;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformance;

@ExtendWith(MockitoExtension.class)
class EntityCopyServiceTest {

  @Captor
  ArgumentCaptor<ScapDetailChild> scapDetailChildCaptor;

  @Captor
  ArgumentCaptor<ProjectDetailsChild> projectDetailChildCaptor;

  @Captor
  ArgumentCaptor<PlannedTenderChild> plannedTenderChildCaptor;

  @Captor
  ArgumentCaptor<PathfinderChild> pathfinderChildCaptor;

  @Mock
  EntityManager entityManager;

  @InjectMocks
  EntityCopyService entityCopyService;

  @ParameterizedTest
  @MethodSource("provideSCAPDetailsChildren")
  void copyChild_SCAPDetailsChild(ScapDetailChild child) {
    var newScapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScapDetailId(5000)
        .build();

    entityCopyService.copyChild(newScapDetail, child);
    verify(entityManager).persist(scapDetailChildCaptor.capture());

    assertThat(scapDetailChildCaptor.getValue())
        .extracting(ScapDetailChild::getId, ScapDetailChild::getScapDetail)
        .containsExactly(null, newScapDetail);
  }

  private static Stream<Arguments> provideSCAPDetailsChildren() {
    var oldScapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
            .withScapDetailId(1000)
            .build();

    return Stream.of(
        Arguments.of(new ProjectPerformance(oldScapDetail, Instant.now())),
        Arguments.of(new ProjectDetails(oldScapDetail, Instant.now())),
        Arguments.of(new PlannedTender(oldScapDetail, Instant.now())),
        Arguments.of(new ActualTender(oldScapDetail, Instant.now())),
        Arguments.of(new ContractingPerformanceOverview(oldScapDetail, Instant.now()))
    );
  }

  @ParameterizedTest
  @MethodSource("provideProjectDetailsChildren")
  void copyChild_ProjectDetailsChild(ProjectDetailsChild child) {
    var newProjectDetails = new ProjectDetails();
    newProjectDetails.setProjectName("New Project");

    entityCopyService.copyChild(newProjectDetails, child);
    verify(entityManager).persist(projectDetailChildCaptor.capture());

    assertThat(projectDetailChildCaptor.getValue())
        .extracting(ProjectDetailsChild::getId, ProjectDetailsChild::getProjectDetails)
        .containsExactly(null, newProjectDetails);
  }

  @Test
  void copyChildren_ProjectDetailsChild() {
    var oldProjectDetail = new ProjectDetails();
    oldProjectDetail.setProjectName("Old Project");

    var newProjectDetails = new ProjectDetails();
    newProjectDetails.setProjectName("New Project");

    var listOfChildren = List.of(
        new ProjectField(oldProjectDetail, 1, Instant.now()),
        new ProjectField(oldProjectDetail, 2, Instant.now()),
        new ProjectField(oldProjectDetail, 3, Instant.now()),
        new ProjectField(oldProjectDetail, 4, Instant.now()),
        new ProjectField(oldProjectDetail, 5, Instant.now())
    );

    entityCopyService.copyChildren(newProjectDetails, listOfChildren);
    verify(entityManager, times(listOfChildren.size())).persist(projectDetailChildCaptor.capture());

    assertThat(projectDetailChildCaptor.getAllValues())
        .extracting(ProjectDetailsChild::getId, ProjectDetailsChild::getProjectDetails)
        .containsExactly(
            tuple(null, newProjectDetails),
            tuple(null, newProjectDetails),
            tuple(null, newProjectDetails),
            tuple(null, newProjectDetails),
            tuple(null, newProjectDetails)
        );
  }

  private static Stream<Arguments> provideProjectDetailsChildren() {
    var oldProjectDetail = new ProjectDetails();
    oldProjectDetail.setProjectName("Old Project");

    return Stream.of(
        Arguments.of(new ProjectDetailType(oldProjectDetail, Instant.now())),
        Arguments.of(new ProjectFacility(oldProjectDetail, Instant.now(), 7)),
        Arguments.of(new ProjectField(oldProjectDetail, 7, Instant.now()))
    );
  }

  @Test
  void copyChild_PathfinderChild() {
    var oldProjectOverview = new PathfinderProjectsOverview();
    var newProjectOverview = new PathfinderProjectsOverview();

    var oldUuid = UUID.randomUUID();
    var oldPathfinderProject = new PathfinderProject(oldUuid);
    oldPathfinderProject.setPathfinderProjectName("TEST");
    oldPathfinderProject.setPathfinderProjectsOverview(oldProjectOverview);

    entityCopyService.copyChild(newProjectOverview, oldPathfinderProject);
    verify(entityManager).persist(pathfinderChildCaptor.capture());

    assertThat(pathfinderChildCaptor.getValue())
        .extracting(
            PathfinderChild::getId,
            PathfinderChild::getPathfinderProjectsOverview)
        .containsExactly(
            null,
            newProjectOverview
        );
  }

  @Test
  void copyChildren_PathfinderChild() {
    var oldProjectOverview = new PathfinderProjectsOverview();
    var newProjectOverview = new PathfinderProjectsOverview();

    var oldPathfinderProject = new PathfinderProject();
    oldPathfinderProject.setPathfinderProjectName("TEST");
    oldPathfinderProject.setPathfinderProjectsOverview(oldProjectOverview);

    var oldPathfinderProject2 = new PathfinderProject();
    oldPathfinderProject2.setPathfinderProjectName("TEST");
    oldPathfinderProject2.setPathfinderProjectsOverview(oldProjectOverview);

    var oldPathfinderProject3 = new PathfinderProject();
    oldPathfinderProject3.setPathfinderProjectName("TEST");
    oldPathfinderProject3.setPathfinderProjectsOverview(oldProjectOverview);

    entityCopyService.copyChildren(newProjectOverview, Set.of(oldPathfinderProject, oldPathfinderProject2, oldPathfinderProject3));
    verify(entityManager, times(3)).persist(pathfinderChildCaptor.capture());

    assertThat(pathfinderChildCaptor.getAllValues())
        .extracting(
            PathfinderChild::getId,
            PathfinderChild::getPathfinderProjectsOverview)
        .containsExactly(
            tuple(null, newProjectOverview),
            tuple(null, newProjectOverview),
            tuple(null, newProjectOverview)
        );
  }

  @ParameterizedTest
  @MethodSource("providePlannedTenderChildren")
  void copyChild_PlannedTenderChild(PlannedTenderChild child) {
    var newPlannedTender = new PlannedTender();

    entityCopyService.copyChild(newPlannedTender, child);
    verify(entityManager).persist(plannedTenderChildCaptor.capture());

    assertThat(plannedTenderChildCaptor.getValue())
        .extracting(PlannedTenderChild::getId, PlannedTenderChild::getPlannedTender)
        .containsExactly(null, newPlannedTender);
  }

  private static Stream<Arguments> providePlannedTenderChildren() {
    var oldPlannedTender = new PlannedTender();

    return Stream.of(
        Arguments.of(new PlannedTenderActivity(oldPlannedTender, Instant.now()))
    );
  }

  @Test
  void copyChildren_PlannedTenderChild() {
    var oldPlannedTender = new PlannedTender();

    var newPlannedTender = new PlannedTender();

    var listOfChildren = List.of(
        new PlannedTenderActivity(oldPlannedTender, Instant.now()),
        new PlannedTenderActivity(oldPlannedTender, Instant.now()),
        new PlannedTenderActivity(oldPlannedTender, Instant.now()),
        new PlannedTenderActivity(oldPlannedTender, Instant.now()),
        new PlannedTenderActivity(oldPlannedTender, Instant.now())
    );

    entityCopyService.copyChildren(newPlannedTender, listOfChildren);
    verify(entityManager, times(listOfChildren.size())).persist(plannedTenderChildCaptor.capture());

    assertThat(plannedTenderChildCaptor.getAllValues())
        .extracting(PlannedTenderChild::getId, PlannedTenderChild::getPlannedTender)
        .containsExactly(
            tuple(null, newPlannedTender),
            tuple(null, newPlannedTender),
            tuple(null, newPlannedTender),
            tuple(null, newPlannedTender),
            tuple(null, newPlannedTender)
        );
  }
}
