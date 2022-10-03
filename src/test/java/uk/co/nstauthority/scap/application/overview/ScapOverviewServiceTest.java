package uk.co.nstauthority.scap.application.overview;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.exception.ScapEntityNotFoundException;

@ExtendWith(MockitoExtension.class)
public class ScapOverviewServiceTest {

  @Mock
  ScapOverviewRepository scapOverviewRepository;

  @InjectMocks
  ScapOverviewService scapOverviewService;

  @Test
  void createScapOverview() {
    var argumentCaptor = ArgumentCaptor.forClass(ScapOverview.class);

    scapOverviewService.createScapOverview(1);

    verify(scapOverviewRepository, times(1)).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue().getOrganisationGroupId()).isEqualTo(1);
  }

  @Test
  void getScapOverviewById_exists() {
    var scapOverview = new ScapOverview(22);

    when(scapOverviewRepository.findById(22)).thenReturn(Optional.of(scapOverview));

    var returnedOverview = scapOverviewService.getScapOverviewById(22);

    assertThat(returnedOverview).isEqualTo(scapOverview);
  }

  @Test
  void getScapOverviewById_doesNotExist() {
    assertThatThrownBy(() -> scapOverviewService.getScapOverviewById(119))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void updateScapOverviewOrganisationGroup() {
    var scapOverview = new ScapOverview(22);
    var argumentCaptor = ArgumentCaptor.forClass(ScapOverview.class);

    scapOverviewService.updateScapOverviewOrganisationGroup(scapOverview, 119);

    verify(scapOverviewRepository, times(1)).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue().getOrganisationGroupId()).isEqualTo(119);
  }
}
