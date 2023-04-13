package uk.co.nstauthority.scap.scap.actualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.utils.ObjectTestingUtil.assertValuesEqual;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import javax.persistence.EntityManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityCopyService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformance;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ExtendWith(MockitoExtension.class)
public class ActualTenderCopyServiceTest {
  EntityManager entityManager = mock(EntityManager.class);

  ActualTenderService actualTenderService = mock(ActualTenderService.class);

  InvitationToTenderParticipantService invitationToTenderParticipantService = mock(InvitationToTenderParticipantService.class);

  ActualTenderActivityCopyService actualTenderActivityCopyService = mock(ActualTenderActivityCopyService.class);

  ContractingPerformanceService contractingPerformanceService = mock(ContractingPerformanceService.class);

  ContractingPerformanceOverviewService contractingPerformanceOverviewService = mock(ContractingPerformanceOverviewService.class);

  EntityCopyService entityCopyService = new EntityCopyService(entityManager);

  ActualTenderCopyService actualTenderCopyService = new ActualTenderCopyService(
      actualTenderService,
      actualTenderActivityCopyService,
      invitationToTenderParticipantService,
      contractingPerformanceService,
      contractingPerformanceOverviewService,
      entityCopyService,
      entityManager);

  @Captor
  ArgumentCaptor<ActualTender> actualTenderCaptor;

  @Captor
  ArgumentCaptor<ContractingPerformance> performanceCaptor;

  @Captor
  ArgumentCaptor<InvitationToTenderParticipant> participantCaptor;

  @Test
  void copyEntity_ActualTender() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldActualTender = new ActualTender();
    oldActualTender.setHasActualTenders(true);
    oldActualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.NO);
    oldActualTender.setScapDetail(oldScapDetail);
    oldActualTender.setId(5000);
    when(actualTenderService.getByScapDetail(oldScapDetail)).thenReturn(oldActualTender);

    actualTenderCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);
    verify(entityManager).persist(actualTenderCaptor.capture());
    var result = actualTenderCaptor.getValue();

    assertThat(result).isNotEqualTo(oldActualTender);
    assertValuesEqual(result, oldActualTender, List.of("id", "scapDetail", "createdTimestamp"));
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result.getId()).isNull();
  }

  @Test
  void copyEntity_ActualTender_DraftUpdate() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldActualTender = new ActualTender();
    oldActualTender.setHasActualTenders(true);
    oldActualTender.setHasMoreActualTenders(HasMoreActualTenderActivities.NO);
    oldActualTender.setScapDetail(oldScapDetail);
    oldActualTender.setId(5000);
    when(actualTenderService.getByScapDetail(oldScapDetail)).thenReturn(oldActualTender);

    actualTenderCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.DRAFT_UPDATE);
    verify(entityManager).persist(actualTenderCaptor.capture());
    verify(actualTenderService).updateHasMoreActualTenders(any(ActualTender.class), eq(null));
    var result = actualTenderCaptor.getValue();

    assertThat(result).isNotEqualTo(oldActualTender);
    assertValuesEqual(result, oldActualTender, List.of("id", "scapDetail", "createdTimestamp"));
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result.getId()).isNull();
  }

  @Test
  void copyEntity_contractingPerformance() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();
    var oldActualTender = new ActualTender();

    var activitiesMap = new HashMap<ActualTenderActivity, ActualTenderActivity>();
    var oldActivity = new ActualTenderActivity();
    var newActivity = new ActualTenderActivity();
    activitiesMap.put(oldActivity, newActivity);

    var oldContractingPerformance = new ContractingPerformance();
    oldContractingPerformance.setOutturnCost(new BigDecimal(5000));
    oldContractingPerformance.setOutturnRationale("TEST");
    oldContractingPerformance.setActualTenderActivity(oldActivity);
    oldContractingPerformance.setId(5000);

    when(actualTenderService.getByScapDetail(oldScapDetail)).thenReturn(oldActualTender);
    when(actualTenderActivityCopyService.copyActualTenderActivities(eq(oldActualTender), any(ActualTender.class)))
        .thenReturn(activitiesMap);
    when(contractingPerformanceService.getAllByActualTenderActivities(activitiesMap.keySet().stream().toList()))
        .thenReturn(Collections.singletonList(oldContractingPerformance));

    actualTenderCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);
    verify(actualTenderActivityCopyService).copyAwardedContracts(activitiesMap);
    verify(entityManager, times(2)).persist(performanceCaptor.capture());

    var result = performanceCaptor.getAllValues().get(1);
    assertValuesEqual(result, oldContractingPerformance, List.of("id", "contractingPerformanceOverview", "actualTenderActivity", "createdTimestamp"));
    assertThat(result.getActualTenderActivity()).isEqualTo(newActivity);
    assertThat(result.getId()).isNull();
  }

  @Test
  void copyEntity_invitationParticipants() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();
    var oldActualTender = new ActualTender();

    var activitiesMap = new HashMap<ActualTenderActivity, ActualTenderActivity>();
    var oldActivity = new ActualTenderActivity();
    var newActivity = new ActualTenderActivity();
    activitiesMap.put(oldActivity, newActivity);

    var oldInvitationParticpant = new InvitationToTenderParticipant();
    oldInvitationParticpant.setOrganisationUnitId(5000);
    oldInvitationParticpant.setBidParticipant(true);
    oldInvitationParticpant.setCompanyName("TEST");

    when(actualTenderService.getByScapDetail(oldScapDetail)).thenReturn(oldActualTender);
    when(actualTenderActivityCopyService.copyActualTenderActivities(eq(oldActualTender), any(ActualTender.class)))
        .thenReturn(activitiesMap);
    when(invitationToTenderParticipantService.getInvitationToTenderParticipants(oldActivity))
        .thenReturn(Collections.singletonList(oldInvitationParticpant));

    actualTenderCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);
    verify(actualTenderActivityCopyService).copyAwardedContracts(activitiesMap);
    verify(entityManager, times(2)).persist(participantCaptor.capture());

    var result = participantCaptor.getValue();
    assertValuesEqual(result, oldInvitationParticpant, List.of("id", "actualTenderActivity", "createdTimestamp"));
    assertThat(result.getActualTenderActivity()).isEqualTo(newActivity);
    assertThat(result.getId()).isNull();
  }
}
