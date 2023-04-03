package uk.co.nstauthority.scap.scap.actualtender;

import java.util.Map;
import javax.persistence.EntityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityCopyService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.copy.CopyService;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Service
class ActualTenderCopyService implements CopyService {

  private final ActualTenderService actualTenderService;

  private final ActualTenderActivityCopyService actualTenderActivityCopyService;

  private final InvitationToTenderParticipantService invitationToTenderParticipantService;

  private final ContractingPerformanceService contractingPerformanceService;

  private final ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  private final EntityCopyService entityCopyService;

  private final EntityManager entityManager;


  @Autowired
  public ActualTenderCopyService(ActualTenderService actualTenderService,
                                 ActualTenderActivityCopyService actualTenderActivityCopyService,
                                 InvitationToTenderParticipantService invitationToTenderParticipantService,
                                 ContractingPerformanceService contractingPerformanceService,
                                 ContractingPerformanceOverviewService contractingPerformanceOverviewService,
                                 EntityCopyService entityCopyService,
                                 EntityManager entityManager) {
    this.actualTenderService = actualTenderService;
    this.actualTenderActivityCopyService = actualTenderActivityCopyService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.contractingPerformanceService = contractingPerformanceService;
    this.contractingPerformanceOverviewService = contractingPerformanceOverviewService;
    this.entityCopyService = entityCopyService;
    this.entityManager = entityManager;
  }

  @Override
  public int runOrder() {
    return 40;
  }

  @Override
  public void copyEntity(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType newScapType) {
    var oldActualTender = actualTenderService.getByScapDetailOrThrow(oldScapDetail);
    var newActualTender = (ActualTender) entityCopyService.copyChild(newScapDetail, oldActualTender);

    var activities = actualTenderActivityCopyService.copyActualTenderActivities(oldActualTender, newActualTender);
    copyInvitationToTenderParticipants(activities);
    actualTenderActivityCopyService.copyAwardedContracts(activities);
    copyContractingPerformance(activities, newScapDetail);
  }

  private void copyInvitationToTenderParticipants(Map<ActualTenderActivity, ActualTenderActivity> activities) {
    activities.forEach((oldActivity, newActivity) ->
        invitationToTenderParticipantService.getInvitationToTenderParticipants(oldActivity)
            .forEach(invitation -> {
              entityManager.detach(invitation);
              invitation.setId(null);
              invitation.setActualTenderActivity(activities.get(oldActivity));
              entityManager.persist(invitation);
            }));
  }

  private void copyContractingPerformance(Map<ActualTenderActivity, ActualTenderActivity> activities, ScapDetail newScapDetail) {
    var newOverview = contractingPerformanceOverviewService.getByScapDetailOrThrow(newScapDetail);
    contractingPerformanceService.getAllByActualTenderActivities(activities.keySet().stream().toList())
        .forEach(performance -> {
          entityManager.detach(performance);
          performance.setId(null);
          performance.setContractingPerformanceOverview(newOverview);
          performance.setActualTenderActivity(activities.get(performance.getActualTenderActivity()));
          entityManager.persist(performance);
        });
  }
}
