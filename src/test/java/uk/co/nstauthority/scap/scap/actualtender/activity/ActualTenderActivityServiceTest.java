package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.tuple;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;

@ExtendWith(MockitoExtension.class)
class ActualTenderActivityServiceTest {

  @Mock
  ActualTenderActivityRepository actualTenderActivityRepository;

  @Mock
  InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;

  Clock clock;

  ActualTenderActivityService actualTenderActivityService;

  @Captor
  private ArgumentCaptor<List<InvitationToTenderParticipant>> invitationToTenderParticipantCaptor;

  private ActualTender actualTender;
  private ActualTenderActivityForm form;

  @BeforeEach
  void setup() {
    actualTender = new ActualTender(42);
    form = new ActualTenderActivityForm();
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("test remuneration model name");
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants("test ITT participant");
    clock = Clock.fixed(Instant.ofEpochSecond(1667316108), ZoneId.systemDefault());
    actualTenderActivityService = new ActualTenderActivityService(
        actualTenderActivityRepository, invitationToTenderParticipantRepository, clock);
  }

  @Test
  void getById_exists() {
    var existingActualTenderActivity = new ActualTenderActivity(37);
    when(actualTenderActivityRepository.findById(existingActualTenderActivity.getId()))
        .thenReturn(Optional.of(existingActualTenderActivity));

    var returnedActualTenderActivity = actualTenderActivityService.getById(existingActualTenderActivity.getId());

    assertThat(returnedActualTenderActivity).isEqualTo(existingActualTenderActivity);
  }

  @Test
  void getById_DoesNotExist_AssertThrows() {
    var nonExistentId = 9;

    when(actualTenderActivityRepository.findById(nonExistentId))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> actualTenderActivityService.getById(nonExistentId))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void getAllByActualTender_VerifyCallsRepository() {
    var actualTender = new ActualTender(43);
    var actualTenderDetail = new ActualTenderActivity(actualTender, Instant.now());
    when(actualTenderActivityRepository.findAllByActualTender(actualTender)).thenReturn(List.of(actualTenderDetail));

    var returnedList = actualTenderActivityService.getAllByActualTender(actualTender);

    assertThat(returnedList).containsExactly(actualTenderDetail);
  }

  @Test
  void createActualTenderActivity_assertSaves() {
    var actualTenderDetailArgumentCaptor = ArgumentCaptor.forClass(ActualTenderActivity.class);

    actualTenderActivityService.createActualTenderActivity(actualTender, form);

    verify(actualTenderActivityRepository).save(actualTenderDetailArgumentCaptor.capture());
    verify(invitationToTenderParticipantRepository).saveAll(invitationToTenderParticipantCaptor.capture());
    verify(invitationToTenderParticipantRepository, never()).deleteAll(any());

    assertThat(actualTenderDetailArgumentCaptor.getValue()).extracting(
        ActualTenderActivity::getScopeTitle,
        ActualTenderActivity::getScopeDescription,
        ActualTenderActivity::getRemunerationModel,
        ActualTenderActivity::getRemunerationModelName,
        ActualTenderActivity::getContractStage,
        ActualTenderActivity::getCreatedTimestamp
    ).containsExactly(
        form.getScopeTitle().getInputValue(),
        form.getScopeDescription().getInputValue(),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getContractStage(),
        clock.instant()
    );

    assertThat(invitationToTenderParticipantCaptor.getValue()).extracting(
        InvitationToTenderParticipant::getCompanyName,
        InvitationToTenderParticipant::getCreatedTimestamp
    ).containsExactly(
        tuple(form.getInvitationToTenderParticipants().getInputValue(), clock.instant())
    );
  }

  @Test
  void updateActualTenderActivity_assertSaves() {
    var actualTenderDetailArgumentCaptor = ArgumentCaptor.forClass(ActualTenderActivity.class);
    var actualTenderDetail = new ActualTenderActivity(actualTender, clock.instant());
    var existingInvitationToTenderParticipants = List.of(
        new InvitationToTenderParticipant()
    );

    when(invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderDetail))
        .thenReturn(existingInvitationToTenderParticipants);

    actualTenderActivityService.updateActualTenderActivity(actualTenderDetail, form);

    verify(actualTenderActivityRepository).save(actualTenderDetailArgumentCaptor.capture());
    verify(invitationToTenderParticipantRepository).saveAll(invitationToTenderParticipantCaptor.capture());
    verify(invitationToTenderParticipantRepository).deleteAll(existingInvitationToTenderParticipants);

    assertThat(actualTenderDetailArgumentCaptor.getValue()).extracting(
        ActualTenderActivity::getScopeTitle,
        ActualTenderActivity::getScopeDescription,
        ActualTenderActivity::getRemunerationModel,
        ActualTenderActivity::getRemunerationModelName,
        ActualTenderActivity::getContractStage,
        ActualTenderActivity::getCreatedTimestamp
    ).containsExactly(
        form.getScopeTitle().getInputValue(),
        form.getScopeDescription().getInputValue(),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getContractStage(),
        clock.instant()
    );

    assertThat(invitationToTenderParticipantCaptor.getValue()).extracting(
        InvitationToTenderParticipant::getCompanyName,
        InvitationToTenderParticipant::getCreatedTimestamp
    ).containsExactly(
        tuple(form.getInvitationToTenderParticipants().getInputValue(), clock.instant())
    );
  }

  @Test
  void hasActualTenderActivity_NoneFound_AssertFalse() {
    when(actualTenderActivityRepository.findFirstByActualTender(actualTender)).thenReturn(Optional.empty());

    assertFalse(actualTenderActivityService.hasActualTenderActivity(actualTender));
  }

  @Test
  void hasActualTenderActivity_IsFound_AssertTrue() {
    when(actualTenderActivityRepository.findFirstByActualTender(actualTender))
        .thenReturn(Optional.of(new ActualTenderActivity()));

    assertTrue(actualTenderActivityService.hasActualTenderActivity(actualTender));
  }

  @Test
  void deleteActualTenderActivity_VerifyDeletes() {
    var actualTenderActivity = new ActualTenderActivity(44);

    actualTenderActivityService.deleteActualTenderActivity(actualTenderActivity);

    verify(actualTenderActivityRepository).delete(actualTenderActivity);
  }
}
