package uk.co.nstauthority.scap.application.overview;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;

@ExtendWith(MockitoExtension.class)
public class ScapOverviewServiceTest {

  @Mock
  ScapOverviewRepository scapOverviewRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ScapOverviewService scapOverviewService;

  @Test
  void createScapOverview() {
    var argumentCaptor = ArgumentCaptor.forClass(ScapOverview.class);

    var scap = scapOverviewService.createScapOverview(1);

    verify(scapOverviewRepository, times(1)).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).isEqualTo(scap);
    assertThat(scap.getOrganisationGroupId()).isEqualTo(1);
  }

  @Test
  void getScapById_exists() {
    var scapOverview = new ScapOverview(22);

    when(scapOverviewRepository.findById(22)).thenReturn(Optional.of(scapOverview));

    var returnedOverview = scapOverviewService.getScapById(22);

    assertThat(returnedOverview).isEqualTo(scapOverview);
  }

  @Test
  void getScapById_doesNotExist() {
    assertThatThrownBy(() -> scapOverviewService.getScapById(119))
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
