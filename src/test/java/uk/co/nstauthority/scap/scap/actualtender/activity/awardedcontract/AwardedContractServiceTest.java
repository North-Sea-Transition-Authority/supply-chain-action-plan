package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
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
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@SuppressWarnings("OptionalGetWithoutIsPresent")
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
    var contractAwardDate = LocalDate.of(2000, 1, 1);
    var contractStartDate = LocalDate.of(2000, 2, 1);
    var contractEndDate = LocalDate.of(2000, 3, 1);
    var otherPaymentTerm = 90;
    var form = new AwardedContractForm();
    form.setPreferredBidderId(bidParticipants.get(0).getId());
    form.setAwardValue("1.23");
    form.setAwardRationale("Test award rationale");
    form.setPreferredBidderCountryId(0);
    form.setContractAwardDate(contractAwardDate);
    form.setContractStartDate(contractStartDate);
    form.setContractEndDate(contractEndDate);
    form.setPaymentTermsRadio(PaymentTermsRadio.OTHER);
    form.setOtherPaymentTerm(String.valueOf(otherPaymentTerm));
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
        AwardedContract::getPreferredBidderCountryId,
        AwardedContract::getContractAwardDate,
        AwardedContract::getPaymentTerms,
        AwardedContract::getForecastExecutionStartDate,
        AwardedContract::getForecastExecutionEndDate
    ).containsExactly(
        awardedContract.getId(),
        bidParticipants.get(0),
        form.getAwardValue().getAsBigDecimal().get(),
        form.getAwardRationale().getInputValue(),
        form.getPreferredBidderCountryId(),
        contractAwardDate,
        otherPaymentTerm,
        contractStartDate,
        contractEndDate
    );
  }

  @Test
  void saveAwardedContract_CreateNew_VerifySaves() {
    var form = new AwardedContractForm();
    var contractAwardDate = LocalDate.of(2000, 1, 1);
    var contractStartDate = LocalDate.of(2000, 2, 1);
    var contractEndDate = LocalDate.of(2000, 3, 1);
    var paymentTerms = PaymentTermsRadio.DAYS_30;
    form.setPreferredBidderId(bidParticipants.get(0).getId());
    form.setAwardValue("1.23");
    form.setAwardRationale("Test award rationale");
    form.setPreferredBidderCountryId(0);
    form.setContractAwardDate(contractAwardDate);
    form.setPaymentTermsRadio(paymentTerms);
    form.setContractStartDate(contractStartDate);
    form.setContractEndDate(contractEndDate);
    var argumentCaptor = ArgumentCaptor.forClass(AwardedContract.class);

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.empty());

    awardedContractService.saveAwardedContract(actualTenderActivity, form, bidParticipants);

    verify(awardedContractRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        AwardedContract::getPreferredBidder,
        AwardedContract::getAwardValue,
        AwardedContract::getAwardRationale,
        AwardedContract::getPreferredBidderCountryId,
        AwardedContract::getCreatedTimestamp,
        AwardedContract::getContractAwardDate,
        AwardedContract::getPaymentTerms,
        AwardedContract::getForecastExecutionStartDate,
        AwardedContract::getForecastExecutionEndDate
    ).containsExactly(
        bidParticipants.get(0),
        form.getAwardValue().getAsBigDecimal().get(),
        form.getAwardRationale().getInputValue(),
        form.getPreferredBidderCountryId(),
        clock.instant(),
        contractAwardDate,
        paymentTerms.getPaymentTerm(),
        contractStartDate,
        contractEndDate
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

  @Test
  void saveAwardedContract_InvalidPaymentTerm_VerifyNeverSaves() {
    var form = new AwardedContractForm();
    var contractAwardDate = LocalDate.of(2000, 1, 1);
    var contractStartDate = LocalDate.of(2000, 2, 1);
    var contractEndDate = LocalDate.of(2000, 3, 1);
    form.setPreferredBidderId(bidParticipants.get(0).getId());
    form.setPaymentTermsRadio(PaymentTermsRadio.OTHER);
    form.setOtherPaymentTerm("NaN");
    form.setAwardValue("1.23");
    form.setContractAwardDate(contractAwardDate);
    form.setContractStartDate(contractStartDate);
    form.setContractEndDate(contractEndDate);

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> awardedContractService.saveAwardedContract(actualTenderActivity, form, bidParticipants))
        .isInstanceOf(ClassCastException.class)
        .hasMessage("Could not cast payment terms [%s] to integer".formatted(form.getOtherPaymentTerm().getInputValue()));

    verify(awardedContractRepository, never()).save(any());
  }

  @Test
  void deleteByActualTenderActivity_IsPresent_VerifyDeletes() {
    var existingContract = new AwardedContract(44);

    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.of(existingContract));

    awardedContractService.deleteByActualTenderActivity(actualTenderActivity);

    verify(awardedContractRepository).delete(existingContract);
  }

  @Test
  void deleteByActualTenderActivity_NotPresent_VerifyNeverDeletes() {
    when(awardedContractRepository.findByActualTenderActivity(actualTenderActivity))
        .thenReturn(Optional.empty());

    awardedContractService.deleteByActualTenderActivity(actualTenderActivity);

    verify(awardedContractRepository, never()).delete(any());
  }

  @Test
  void getByActualTenderActivityIn() {
    var actualTenderingActivities = List.of(new ActualTenderActivity(173));
    var awardedContracts = List.of(new AwardedContract(5411));

    when(awardedContractRepository.findByActualTenderActivityIn(actualTenderingActivities))
        .thenReturn(awardedContracts);

    var returnedContracts = awardedContractService.getByActualTenderActivityIn(actualTenderingActivities);

    assertThat(returnedContracts).isEqualTo(awardedContracts);
  }

  @Test
  void removePreferredBidder_VerifyCalls() {
    var awardedContract = AwardedContractBuilder.newBuilder().build();

    awardedContractService.removePreferredBidder(awardedContract);

    verify(awardedContractRepository).save(awardedContract);
    assertThat(awardedContract.getPreferredBidder()).isNull();
  }
}
