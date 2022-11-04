package uk.co.nstauthority.scap.application.actualtender.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.application.actualtender.ActualTender;

@ExtendWith(MockitoExtension.class)
class ActualTenderDetailServiceTest {

  @Mock
  ActualTenderDetailRepository actualTenderDetailRepository;

  @Mock
  InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;

  Clock clock;

  ActualTenderDetailService actualTenderDetailService;

  @Captor
  private ArgumentCaptor<List<InvitationToTenderParticipant>> invitationToTenderParticipantCaptor;

  private ActualTender actualTender;
  private ActualTenderDetailForm form;

  @BeforeEach
  void setup() {
    actualTender = new ActualTender(42);
    form = new ActualTenderDetailForm();
    form.setScopeTitle("test scope title");
    form.setScopeDescription("test scope description");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("test remuneration model name");
    form.setContractStage(ContractStage.CONTRACT_AWARDED);
    form.setInvitationToTenderParticipants("test ITT participant");
    clock = Clock.fixed(Instant.ofEpochSecond(1667316108), ZoneId.systemDefault());
    actualTenderDetailService = new ActualTenderDetailService(
        actualTenderDetailRepository, invitationToTenderParticipantRepository, clock);
  }

  @Test
  void createActualTenderDetail_assertSaves() {
    var actualTenderDetailArgumentCaptor = ArgumentCaptor.forClass(ActualTenderDetail.class);

    actualTenderDetailService.createActualTenderDetail(actualTender, form);

    verify(actualTenderDetailRepository, times(1)).save(actualTenderDetailArgumentCaptor.capture());
    verify(invitationToTenderParticipantRepository, times(1)).saveAll(invitationToTenderParticipantCaptor.capture());
    verify(invitationToTenderParticipantRepository, never()).deleteAll(any());

    assertThat(actualTenderDetailArgumentCaptor.getValue()).extracting(
        ActualTenderDetail::getScopeTitle,
        ActualTenderDetail::getScopeDescription,
        ActualTenderDetail::getRemunerationModel,
        ActualTenderDetail::getRemunerationModelName,
        ActualTenderDetail::getContractStage,
        ActualTenderDetail::getCreatedTimestamp
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
  void updateActualTenderDetail_assertSaves() {
    var actualTenderDetailArgumentCaptor = ArgumentCaptor.forClass(ActualTenderDetail.class);
    var actualTenderDetail = new ActualTenderDetail(actualTender, clock.instant());
    var existingInvitationToTenderParticipants = List.of(
        new InvitationToTenderParticipant()
    );

    when(invitationToTenderParticipantRepository.findAllByActualTenderDetail(actualTenderDetail))
        .thenReturn(existingInvitationToTenderParticipants);

    actualTenderDetailService.updateActualTenderDetail(actualTenderDetail, form);

    verify(actualTenderDetailRepository).save(actualTenderDetailArgumentCaptor.capture());
    verify(invitationToTenderParticipantRepository).saveAll(invitationToTenderParticipantCaptor.capture());
    verify(invitationToTenderParticipantRepository).deleteAll(existingInvitationToTenderParticipants);

    assertThat(actualTenderDetailArgumentCaptor.getValue()).extracting(
        ActualTenderDetail::getScopeTitle,
        ActualTenderDetail::getScopeDescription,
        ActualTenderDetail::getRemunerationModel,
        ActualTenderDetail::getRemunerationModelName,
        ActualTenderDetail::getContractStage,
        ActualTenderDetail::getCreatedTimestamp
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
}
