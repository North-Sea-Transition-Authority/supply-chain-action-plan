package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.OrganisationUnit;
import uk.co.nstauthority.scap.energyportal.OrganisationUnitService;
import uk.co.nstauthority.scap.fds.searchselector.ManualEntryUtil;

@ExtendWith(MockitoExtension.class)
class InvitationToTenderParticipantServiceTest {

  @Mock
  InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;

  @Mock
  Clock clock;

  @Mock
  OrganisationUnitService organisationUnitService;

  @InjectMocks
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Captor
  ArgumentCaptor<List<InvitationToTenderParticipant>> participantListArgumentCaptor;

  @Captor
  ArgumentCaptor<Set<InvitationToTenderParticipant>> participantSetArgumentCaptor;

  @Captor
  ArgumentCaptor<Set<InvitationToTenderParticipant>> participantSetArgumentCaptor2;

  @Test
  void getInvitationToTenderParticipants() {
    var actualTenderActivity = new ActualTenderActivity(37);
    var participant = new InvitationToTenderParticipant(actualTenderActivity, Instant.now(), "test company name");

    when(invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderActivity))
        .thenReturn(List.of(participant));

    var returnedParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderActivity);

    assertThat(returnedParticipants).containsExactly(participant);
  }

  @Test
  void getBidParticipants() {
    var actualTenderActivity = new ActualTenderActivity(141);
    var participant1 = new InvitationToTenderParticipant(actualTenderActivity, Instant.now(), "test company name");
    participant1.setBidParticipant(true);
    var participant2 = new InvitationToTenderParticipant(actualTenderActivity, Instant.now(), "test company name");
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

  @Test
  void updateInvitationToTenderParticipants_VerifySaveAndDelete() {
    var actualTenderActivity = new ActualTenderActivityBuilder().build();
    var keptParticipant = new InvitationToTenderParticipantBuilder()
        .withActualTenderActivity(actualTenderActivity)
        .withCompanyName("existing company name")
        .build();
    var keptParticipantFromPortal = new InvitationToTenderParticipantBuilder()
        .withActualTenderActivity(actualTenderActivity)
        .withCompanyName("kept EPA org unit name")
        .withOrganisationUnitId(17)
        .build();
    var removedParticipant = new InvitationToTenderParticipantBuilder()
        .withActualTenderActivity(actualTenderActivity)
        .withCompanyName("removed company name")
        .build();
    var removedParticipantFromPortal = new InvitationToTenderParticipantBuilder()
        .withActualTenderActivity(actualTenderActivity)
        .withCompanyName("removed company name")
        .withOrganisationUnitId(1)
        .build();
    var addedParticipantName = "new company name";
    var addedOrganisationUnitId = 5;
    var addedOrganisation = OrganisationUnit.newBuilder()
        .organisationUnitId(addedOrganisationUnitId)
        .name("portal company name")
        .build();

    var participantsFromForm = List.of(
        ManualEntryUtil.addFreeTextPrefix(keptParticipant.getCompanyName()),
        String.valueOf(keptParticipantFromPortal.getOrganisationUnitId()),
        ManualEntryUtil.addFreeTextPrefix(addedParticipantName),
        String.valueOf(addedOrganisationUnitId)
    );
    var currentInstant = Instant.now();

    when(clock.instant()).thenReturn(currentInstant);
    when(invitationToTenderParticipantRepository.findAllByActualTenderActivity(actualTenderActivity))
        .thenReturn(List.of(keptParticipant, keptParticipantFromPortal, removedParticipant, removedParticipantFromPortal));
    when(organisationUnitService.findAllByIds(
        Collections.singletonList(addedOrganisationUnitId),
        InvitationToTenderParticipantService.ORGANISATION_UNIT_REQUEST_PURPOSE))
        .thenReturn(Collections.singletonList(addedOrganisation));

    invitationToTenderParticipantService.updateInvitationToTenderParticipants(actualTenderActivity, participantsFromForm);

    verify(invitationToTenderParticipantRepository).saveAll(participantSetArgumentCaptor.capture());
    verify(invitationToTenderParticipantRepository).deleteAll(participantSetArgumentCaptor2.capture());

    assertThat(participantSetArgumentCaptor2.getValue()).containsExactlyInAnyOrder(
        removedParticipant,
        removedParticipantFromPortal
    );
    assertThat(participantSetArgumentCaptor.getValue()).extracting(
        InvitationToTenderParticipant::getCompanyName,
        InvitationToTenderParticipant::getCreatedTimestamp,
        InvitationToTenderParticipant::getOrganisationUnitId
    ).containsExactlyInAnyOrder(
        tuple(addedParticipantName, currentInstant, null),
        tuple(addedOrganisation.getName(), currentInstant, addedOrganisationUnitId)
    );
  }
}
