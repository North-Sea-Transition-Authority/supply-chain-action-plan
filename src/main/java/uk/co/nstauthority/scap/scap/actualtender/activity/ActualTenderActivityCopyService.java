package uk.co.nstauthority.scap.scap.actualtender.activity;

import jakarta.persistence.EntityManager;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.beans.BeanUtils;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;

@Service
public class ActualTenderActivityCopyService {

  private final ActualTenderActivityService actualTenderActivityService;

  private final ActualTenderActivityRepository actualTenderActivityRepository;

  private final AwardedContractService awardedContractService;

  private final InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;

  private final EntityManager entityManager;

  public ActualTenderActivityCopyService(ActualTenderActivityService actualTenderActivityService,
                                         ActualTenderActivityRepository actualTenderActivityRepository,
                                         AwardedContractService awardedContractService,
                                         InvitationToTenderParticipantRepository invitationToTenderParticipantRepository,
                                         EntityManager entityManager) {
    this.actualTenderActivityService = actualTenderActivityService;
    this.actualTenderActivityRepository = actualTenderActivityRepository;
    this.awardedContractService = awardedContractService;
    this.invitationToTenderParticipantRepository = invitationToTenderParticipantRepository;
    this.entityManager = entityManager;
  }

  public Map<ActualTenderActivity, ActualTenderActivity> copyActualTenderActivities(ActualTender oldActualTender,
                                                                                    ActualTender newActualtender) {
    var oldActualActivities = actualTenderActivityService.getAllByActualTender(oldActualTender);

    return oldActualActivities
        .stream()
        .collect(Collectors.toMap(actualTenderActivity -> actualTenderActivity,
            actualTenderActivity -> copyActualTenderActivity(actualTenderActivity, newActualtender)));
  }

  private ActualTenderActivity copyActualTenderActivity(ActualTenderActivity oldActualTenderActivity,
                                                        ActualTender newActualtender) {
    var newActualTenderingActivity = BeanUtils.instantiateClass(ActualTenderActivity.class);
    BeanUtils.copyProperties(oldActualTenderActivity, newActualTenderingActivity);
    newActualTenderingActivity.setId(null);
    newActualTenderingActivity.setActualTender(newActualtender);
    return actualTenderActivityRepository.save(newActualTenderingActivity);
  }

  public void copyAwardedContracts(Map<ActualTenderActivity, ActualTenderActivity> activities) {
    awardedContractService.getByActualTenderActivityIn(activities.keySet().stream().toList())
        .forEach(contract -> {
          var newAwardedContract = BeanUtils.instantiateClass(AwardedContract.class);
          BeanUtils.copyProperties(contract, newAwardedContract);
          newAwardedContract.setId(null);
          newAwardedContract.setActualTenderActivity(activities.get(contract.getActualTenderActivity()));

          newAwardedContract.setPreferredBidder(invitationToTenderParticipantRepository
              .getByOrganisationUnitIdAndActualTenderActivity(
                  contract.getPreferredBidder().getOrganisationUnitId(),
                  activities.get(contract.getActualTenderActivity())));
          entityManager.persist(newAwardedContract);
        });
  }
}
