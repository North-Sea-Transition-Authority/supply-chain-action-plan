package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class InvitationToTenderParticipantServiceTest {

  @Mock
  InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;

  @InjectMocks
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Captor
  ArgumentCaptor<List<InvitationToTenderParticipant>> participantListArgumentCaptor;

  @Test
  void getInvitationToTenderParticipants() {
    var actualTenderActivity = new ActualTenderActivity(37);
    var participant = new InvitationToTenderParticipant(actualTenderActivity, Instant.now());
    participant.setCompanyName("test company name");

    when(invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderActivity))
        .thenReturn(List.of(participant));

    var returnedParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderActivity);

    assertThat(returnedParticipants).containsExactly(participant);
  }

  @Test
  void getBidParticipants() {
    var actualTenderActivity = new ActualTenderActivity(141);
    var participant1 = new InvitationToTenderParticipant(actualTenderActivity, Instant.now());
    participant1.setCompanyName("test company name");
    participant1.setBidParticipant(true);
    var participant2 = new InvitationToTenderParticipant(actualTenderActivity, Instant.now());
    participant2.setCompanyName("test company name");
    participant2.setBidParticipant(false);

    when(invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderActivity))
        .thenReturn(List.of(participant1, participant2));

    var bidParticipants = invitationToTenderParticipantService.getBidParticipants(actualTenderActivity);

    assertThat(bidParticipants).containsExactly(participant1);
  }

  @Test
  void updateBidParticipants() {
    var invitationToTenderParticipant1 = new InvitationToTenderParticipant(37);
    invitationToTenderParticipant1.setCompanyName("company name 1");
    var invitationToTenderParticipant2 = new InvitationToTenderParticipant(38);
    invitationToTenderParticipant2.setCompanyName("company name 2");
    var invitationToTenderParticipants = List.of(
        invitationToTenderParticipant1, invitationToTenderParticipant2
    );
    var selectedBidParticipantIds = List.of(
        invitationToTenderParticipant1.getId()
    );

    invitationToTenderParticipantService.updateBidParticipants(invitationToTenderParticipants, selectedBidParticipantIds);

    verify(invitationToTenderParticipantRepository).saveAll(participantListArgumentCaptor.capture());

    var savedParticipants = participantListArgumentCaptor.getValue();

    assertThat(savedParticipants).extracting(
        InvitationToTenderParticipant::getId,
        InvitationToTenderParticipant::getCompanyName,
        InvitationToTenderParticipant::getBidParticipant
    ).containsExactly(
        tuple(invitationToTenderParticipant1.getId(), invitationToTenderParticipant1.getCompanyName(), Boolean.TRUE),
        tuple(invitationToTenderParticipant2.getId(), invitationToTenderParticipant2.getCompanyName(), Boolean.FALSE)
    );
  }

  @Test
  void deleteAllByActualTenderActivity_VerifyDeletes() {
    var actualTenderActivity = new ActualTenderActivity(44);
    var participant1 = new InvitationToTenderParticipant(441);
    participant1.setCompanyName("company 1");
    var participant2 = new InvitationToTenderParticipant(442);
    participant2.setCompanyName("company 2");
    var participants = List.of(participant1, participant2);

    when(invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderActivity))
        .thenReturn(participants);

    invitationToTenderParticipantService.deleteAllByActualTenderActivity(actualTenderActivity);

    verify(invitationToTenderParticipantRepository).deleteAll(participants);
  }

  @Test
  void getInvitationToTenderParticipantsForActivities() {
    var actualTenderingActivities = List.of(new ActualTenderActivity(173));
    var participant1 = new InvitationToTenderParticipant(441);
    var participant2 = new InvitationToTenderParticipant(442);
    var participants = List.of(participant1, participant2);

    when(invitationToTenderParticipantRepository.findAllByActualTenderActivityIn(actualTenderingActivities))
        .thenReturn(participants);

    var returnedParticipants = invitationToTenderParticipantService.getInvitationToTenderParticipantsForActivities(actualTenderingActivities);

    assertThat(returnedParticipants).isEqualTo(participants);
  }
}
