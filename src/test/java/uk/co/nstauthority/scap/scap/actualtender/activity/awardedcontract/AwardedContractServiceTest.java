package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
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
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@ExtendWith(MockitoExtension.class)
class AwardedContractServiceTest {

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667916210), ZoneId.systemDefault());

  @Mock
  AwardedContractRepository awardedContractRepository;

  @InjectMocks
  AwardedContractService awardedContractService;

  private ActualTenderActivity actualTenderActivity;
  private List<InvitationToTenderParticipant> bidParticipants;

  @BeforeEach
  void setup() {
    actualTenderActivity = new ActualTenderActivity(141);
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);
    bidParticipants = List.of(
        new InvitationToTenderParticipant(1)
    );
  }

  @Test
  void getByActualTenderActivity() {
    var awardedContract = new AwardedContract(27);

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.of(awardedContract));

    var returnedAwardedContract = awardedContractService.getByActualTenderActivity(actualTenderActivity);

    assertThat(returnedAwardedContract).contains(awardedContract);
  }

  @Test
  void saveAwardedContract_UpdateExisting_VerifySaves() {
    var awardedContract = new AwardedContract(27);
    var form = new AwardedContractForm();
    form.setPreferredBidderId(bidParticipants.get(0).getId());
    form.setAwardValue("1.23");
    form.setAwardRationale("Test award rationale");
    form.setPreferredBidderLocation(0);
    var argumentCaptor = ArgumentCaptor.forClass(AwardedContract.class);

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.of(awardedContract));

    awardedContractService.saveAwardedContract(actualTenderActivity, form, bidParticipants);

    verify(awardedContractRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        AwardedContract::getId,
        AwardedContract::getPreferredBidder,
        AwardedContract::getAwardValue,
        AwardedContract::getAwardRationale,
        AwardedContract::getPreferredBidderLocation
    ).containsExactly(
        awardedContract.getId(),
        bidParticipants.get(0),
        form.getAwardValue().getAsBigDecimal().get(),
        form.getAwardRationale().getInputValue(),
        form.getPreferredBidderLocation()
    );
  }

  @Test
  void saveAwardedContract_CreateNew_VerifySaves() {
    var form = new AwardedContractForm();
    form.setPreferredBidderId(bidParticipants.get(0).getId());
    form.setAwardValue("1.23");
    form.setAwardRationale("Test award rationale");
    form.setPreferredBidderLocation(0);
    var argumentCaptor = ArgumentCaptor.forClass(AwardedContract.class);

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.empty());

    awardedContractService.saveAwardedContract(actualTenderActivity, form, bidParticipants);

    verify(awardedContractRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        AwardedContract::getPreferredBidder,
        AwardedContract::getAwardValue,
        AwardedContract::getAwardRationale,
        AwardedContract::getPreferredBidderLocation,
        AwardedContract::getCreatedTimestamp
    ).containsExactly(
        bidParticipants.get(0),
        form.getAwardValue().getAsBigDecimal().get(),
        form.getAwardRationale().getInputValue(),
        form.getPreferredBidderLocation(),
        clock.instant()
    );
  }

  @Test
  void saveAwardedContract_NonExistentParticipant_VerifyNeverSaves() {
    var form = new AwardedContractForm();
    form.setPreferredBidderId(999);

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> awardedContractService.saveAwardedContract(actualTenderActivity, form, bidParticipants))
        .isInstanceOf(ScapEntityNotFoundException.class)
        .hasMessage(String.format("Could not find bid participant with ID [%d]", form.getPreferredBidderId()));

    verify(awardedContractRepository, never()).save(any());
  }

  @Test
  void saveAwardedContract_InvalidAwardValue_VerifyNeverSaves() {
    var form = new AwardedContractForm();
    form.setPreferredBidderId(bidParticipants.get(0).getId());
    form.setAwardValue("NaN");

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> awardedContractService.saveAwardedContract(actualTenderActivity, form, bidParticipants))
        .isInstanceOf(ClassCastException.class)
        .hasMessage(String.format("Could not cast %s to BigDecimal", form.getAwardValue().getInputValue()));

    verify(awardedContractRepository, never()).save(any());
  }
}
