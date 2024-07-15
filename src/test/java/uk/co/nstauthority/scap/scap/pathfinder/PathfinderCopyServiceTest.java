package uk.co.nstauthority.scap.scap.pathfinder;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.utils.ObjectTestingUtil.assertValuesEqual;

import jakarta.persistence.EntityManager;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
class PathfinderCopyServiceTest {

  EntityManager entityManager = mock(EntityManager.class);
  PathfinderService pathfinderService = mock(PathfinderService.class);
  PathfinderProjectsOverviewRepository pathfinderProjectsOverviewRepository = mock(PathfinderProjectsOverviewRepository.class);
  EntityCopyService entityCopyService = new EntityCopyService(entityManager);

  PathfinderCopyService pathfinderCopyService = new PathfinderCopyService(
      pathfinderService,
      pathfinderProjectsOverviewRepository,
      entityCopyService,
      entityManager);

  @Captor
  ArgumentCaptor<PathfinderProjectsOverview> pathfinderOverviewCaptor;

  @Test
  void copyEntity_PathfinderOverview() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldPathfinderOverview = new PathfinderProjectsOverview();
    oldPathfinderOverview.setId(UUID.randomUUID());
    oldPathfinderOverview.setHasRelatedPathfinderProjects(true);
    oldPathfinderOverview.setScapDetail(oldScapDetail);

    when(pathfinderProjectsOverviewRepository.findByScapDetail(oldScapDetail)).thenReturn(
        Optional.of(oldPathfinderOverview));
    pathfinderCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);
    verify(entityManager).persist(pathfinderOverviewCaptor.capture());

    var result = pathfinderOverviewCaptor.getValue();
    assertThat(result).isNotEqualTo(oldPathfinderOverview);
    assertValuesEqual(result, oldPathfinderOverview, List.of("id", "scapDetail", "createdTimestamp"));
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result.getId()).isNull();
  }
}
