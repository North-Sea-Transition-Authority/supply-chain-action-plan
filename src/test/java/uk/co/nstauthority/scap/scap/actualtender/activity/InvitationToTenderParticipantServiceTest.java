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
}
