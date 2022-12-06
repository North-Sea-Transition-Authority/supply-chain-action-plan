package uk.co.nstauthority.scap.scap.scap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;

@ExtendWith(MockitoExtension.class)
class ScapServiceTest {

  @Mock
  ScapRepository scapRepository;

  Clock clock;

  Instant fixedInstant;

  ScapService scapService;

  @BeforeEach
  void setup() {
    fixedInstant = Instant.ofEpochSecond(1667576106);
    clock = Clock.fixed(fixedInstant, ZoneId.systemDefault());
    scapService = new ScapService(scapRepository, clock);
  }

  @Test
  void createScap() {
    var argumentCaptor = ArgumentCaptor.forClass(Scap.class);
    var existingScaps = 58;
    var organisationGroupId = 1;
    var fixedYear = LocalDate.ofInstant(fixedInstant, clock.getZone()).getYear();
    var startOfYear = ZonedDateTime.of(
        fixedYear, 1, 1,
        0, 0, 0, 0,
        clock.getZone());
    var endOfYear = startOfYear.plusYears(1).minusNanos(1000);

    when(scapRepository.countByCreatedTimestampBetween(startOfYear.toInstant(), endOfYear.toInstant()))
        .thenReturn(existingScaps);

    var scap = scapService.createScap(organisationGroupId);

    verify(scapRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).isEqualTo(scap);
    assertThat(scap).extracting(
        Scap::getOrganisationGroupId,
        Scap::getCreatedTimestamp,
        Scap::getReference
    ).containsExactly(
        organisationGroupId,
        fixedInstant,
        "SCAP/%d/%d".formatted(fixedYear, existingScaps + 1)
    );
  }

  @Test
  void createScap_NoExistingScapsInYear_AssertCorrectReference() {
    var argumentCaptor = ArgumentCaptor.forClass(Scap.class);
    var organisationGroupId = 1;
    var fixedYear = LocalDate.ofInstant(fixedInstant, clock.getZone()).getYear();
    var startOfYear = ZonedDateTime.of(
        fixedYear, 1, 1,
        0, 0, 0, 0,
        clock.getZone());
    var endOfYear = startOfYear.plusYears(1).minusNanos(1000);

    when(scapRepository.countByCreatedTimestampBetween(startOfYear.toInstant(), endOfYear.toInstant()))
        .thenReturn(0);

    var scap = scapService.createScap(organisationGroupId);

    verify(scapRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue().getReference()).isEqualTo("SCAP/%d/1".formatted(fixedYear));
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
