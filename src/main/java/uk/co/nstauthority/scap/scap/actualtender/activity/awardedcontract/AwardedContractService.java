package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import jakarta.transaction.Transactional;
import java.time.Clock;
import java.util.List;
import java.util.Optional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;

@Service
public class AwardedContractService {

  private final AwardedContractRepository awardedContractRepository;
  private final Clock clock;

  @Autowired
  AwardedContractService(AwardedContractRepository awardedContractRepository, Clock clock) {
    this.awardedContractRepository = awardedContractRepository;
    this.clock = clock;
  }

  public Optional<AwardedContract> getByActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    return awardedContractRepository.findByActualTenderActivity(actualTenderActivity);
  }

  @Transactional
  public void saveAwardedContract(ActualTenderActivity actualTenderActivity, AwardedContractForm form,
                           List<InvitationToTenderParticipant> bidParticipants) {
    var awardedContract = getByActualTenderActivity(actualTenderActivity)
        .orElse(new AwardedContract(actualTenderActivity, clock.instant()));
    var preferredBidder = bidParticipants.stream()
        .filter(participant -> participant.getId().equals(form.getPreferredBidderId()))
        .findFirst()
        .orElseThrow(() -> new ScapEntityNotFoundException(
            String.format("Could not find bid participant with ID [%d]", form.getPreferredBidderId())));
    var awardValue = form.getAwardValue().getAsBigDecimal()
        .orElseThrow(() -> new ClassCastException(
            String.format("Could not cast %s to BigDecimal", form.getAwardValue().getInputValue())));
    var contractAwardDate = form.getContractAwardDate().getAsLocalDate()
        .orElseThrow(() -> new ClassCastException("Could not cast contractAwardDate to LocalDate"));
    var contractStartDate = form.getContractStartDate().getAsLocalDate()
        .orElseThrow(() -> new ClassCastException("Could not cast contractStartDate to LocalDate"));
    var contractEndDate = form.getContractEndDate().getAsLocalDate()
        .orElseThrow(() -> new ClassCastException("Could not cast contractEndDate to LocalDate"));

    awardedContract.setPreferredBidder(preferredBidder);
    awardedContract.setAwardValue(awardValue);
    awardedContract.setAwardRationale(form.getAwardRationale().getInputValue());
    awardedContract.setPreferredBidderCountryId(form.getPreferredBidderCountryId());
    awardedContract.setContractAwardDate(contractAwardDate);
    if (PaymentTermsRadio.OTHER.equals(form.getPaymentTermsRadio())) {
      var paymentTerms = form.getOtherPaymentTerm().getAsInteger()
          .orElseThrow(() -> new ClassCastException("Could not cast payment terms [%s] to integer"
              .formatted(form.getOtherPaymentTerm().getInputValue())));
      awardedContract.setPaymentTerms(paymentTerms);
    } else {
      awardedContract.setPaymentTerms(form.getPaymentTermsRadio().getPaymentTerm());
    }
    awardedContract.setForecastExecutionStartDate(contractStartDate);
    awardedContract.setForecastExecutionEndDate(contractEndDate);

    awardedContractRepository.save(awardedContract);
  }

  @Transactional
  public void deleteByActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    var awardedContract = getByActualTenderActivity(actualTenderActivity);
    awardedContract.ifPresent(awardedContractRepository::delete);
  }

  public List<AwardedContract> getByActualTenderActivityIn(List<ActualTenderActivity> activities) {
    return awardedContractRepository.findByActualTenderActivityIn(activities);
  }

  @Transactional
  public void removePreferredBidder(AwardedContract awardedContract) {
    awardedContract.setPreferredBidder(null);
    awardedContractRepository.save(awardedContract);
  }
}
