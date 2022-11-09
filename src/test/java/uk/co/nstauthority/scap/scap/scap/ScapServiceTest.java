package uk.co.nstauthority.scap.scap.scap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
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
class ScapServiceTest {

  @Mock
  ScapRepository scapRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ScapService scapService;

  @Test
  void createScap() {
    var argumentCaptor = ArgumentCaptor.forClass(Scap.class);

    var scap = scapService.createScap(1);

    verify(scapRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).isEqualTo(scap);
    assertThat(scap.getOrganisationGroupId()).isEqualTo(1);
  }

  @Test
  void getScapById_exists() {
    var scapOverview = new Scap(22);

    when(scapRepository.findById(scapOverview.getId())).thenReturn(Optional.of(scapOverview));

    var returnedOverview = scapService.getScapById(scapOverview.getId());

    assertThat(returnedOverview).isEqualTo(scapOverview);
  }

  @Test
  void getScapById_doesNotExist() {
    assertThatThrownBy(() -> scapService.getScapById(119))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void updateScapOrganisationGroup() {
    var scapOverview = new Scap(22);
    var argumentCaptor = ArgumentCaptor.forClass(Scap.class);
    var organisationGroupId = 119;

    scapService.updateScapOrganisationGroup(scapOverview, organisationGroupId);

    verify(scapRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue().getOrganisationGroupId()).isEqualTo(organisationGroupId);
  }
}
