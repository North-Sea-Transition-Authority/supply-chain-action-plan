package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import java.time.Clock;
import java.util.List;
import java.util.Optional;
import javax.transaction.Transactional;
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

  void saveAwardedContract(ActualTenderActivity actualTenderActivity, AwardedContractForm form,
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

    awardedContract.setPreferredBidder(preferredBidder);
    awardedContract.setAwardValue(awardValue);
    awardedContract.setAwardRationale(form.getAwardRationale().getInputValue());
    awardedContract.setPreferredBidderLocation(form.getPreferredBidderLocation());

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
}
