package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.utils.ObjectTestingUtil.assertValuesEqual;

import jakarta.persistence.EntityManager;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Random;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;

@ExtendWith(MockitoExtension.class)
class ActualTenderActivityCopyServiceTest {
  @Mock
  EntityManager entityManager;

  @Mock
  ActualTenderActivityRepository actualTenderActivityRepository;

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @Mock
  AwardedContractService awardedContractService;

  @Mock
  InvitationToTenderParticipantRepository invitationToTenderParticipantRepository;

  @Captor
  ArgumentCaptor<ActualTenderActivity> activityCaptor;

  @Captor
  ArgumentCaptor<AwardedContract> contractCaptor;

  @InjectMocks
  ActualTenderActivityCopyService actualTenderActivityCopyService;

  @Test
  void copyService_copyChild_actualTenderingActivity() {
    var oldActualTender = new ActualTender();
    var newActualTender = new ActualTender();

    var oldActualTenderActivity = getOldActualTenderingActivity(oldActualTender, 5000);
    when(actualTenderActivityService.getAllByActualTender(oldActualTender)).thenReturn(List.of(getOldActualTenderingActivity(oldActualTender, 5000)));
    when(actualTenderActivityRepository.save(any(ActualTenderActivity.class))).thenReturn(new ActualTenderActivity());

    var resultMap = actualTenderActivityCopyService.copyActualTenderActivities(oldActualTender, newActualTender);
    verify(actualTenderActivityRepository).save(activityCaptor.capture());

    var result = activityCaptor.getValue();
    assertThat(result.getActualTender()).isEqualTo(newActualTender);
    assertValuesEqual(result, oldActualTenderActivity, List.of("id", "actualTender", "createdTimestamp"));
    assertThat(result.getId()).isNotEqualTo(oldActualTenderActivity.getId());
    assertThat(resultMap.keySet()).doesNotContainAnyElementsOf(resultMap.values());
  }

  @Test
  void copyService_copyChild_awardedContracts() {
    var oldActualTenderActivity = new ActualTenderActivity();
    var newActualTenderActvity = new ActualTenderActivity();
    var map = new HashMap<ActualTenderActivity, ActualTenderActivity>();
    map.put(oldActualTenderActivity, newActualTenderActvity);

    var oldPreferredBidder = new InvitationToTenderParticipant();
    oldPreferredBidder.setOrganisationUnitId(11111);
    var newPreferredBidder = new InvitationToTenderParticipant();

    var oldAwardedContract = new AwardedContract();
    oldAwardedContract.setId(1000);
    oldAwardedContract.setPreferredBidder(oldPreferredBidder);
    oldAwardedContract.setActualTenderActivity(oldActualTenderActivity);
    oldAwardedContract.setAwardRationale("Test");
    oldAwardedContract.setPreferredBidderCountryId(1000);
    oldAwardedContract.setAwardValue(new BigDecimal(5000));
    oldAwardedContract.setPaymentTerms(1);

    when(invitationToTenderParticipantRepository
        .getByOrganisationUnitIdAndCompanyNameAndActualTenderActivity(
            oldPreferredBidder.getOrganisationUnitId(),
            oldPreferredBidder.getCompanyName(),
            oldActualTenderActivity))
        .thenReturn(newPreferredBidder);
    when(awardedContractService.getByActualTenderActivityIn(map.keySet().stream().toList()))
        .thenReturn(List.of(oldAwardedContract));

    actualTenderActivityCopyService.copyAwardedContracts(map);
    verify(entityManager).persist(contractCaptor.capture());

    var result = contractCaptor.getValue();
    assertThat(result)
        .extracting(
            AwardedContract::getActualTenderActivity,
            AwardedContract::getPreferredBidder
            )
        .containsExactly(
            newActualTenderActvity,
            newPreferredBidder
        );
    assertValuesEqual(result, oldAwardedContract, List.of("id", "actualTenderActivity", "preferredBidder", "createdTimestamp"));
    assertThat(result.getId()).isNotEqualTo(oldAwardedContract.getId());
  }

  @Test
  void copyService_copyChildren_actualTenderingActivity() {
    var rand = new Random();
    var oldActualTender = new ActualTender();
    var newActualTender = new ActualTender();

    when(actualTenderActivityService.getAllByActualTender(oldActualTender))
        .thenReturn(List.of(
            getOldActualTenderingActivity(oldActualTender, rand.nextInt()),
            getOldActualTenderingActivity(oldActualTender, rand.nextInt()),
            getOldActualTenderingActivity(oldActualTender, rand.nextInt()),
            getOldActualTenderingActivity(oldActualTender, rand.nextInt())));

    when(actualTenderActivityRepository.save(any(ActualTenderActivity.class))).thenReturn(new ActualTenderActivity());
    actualTenderActivityCopyService.copyActualTenderActivities(oldActualTender, newActualTender);
    verify(actualTenderActivityRepository, times(4)).save(any(ActualTenderActivity.class));
  }

  private ActualTenderActivity getOldActualTenderingActivity(ActualTender oldActualTender, int id) {
    var oldActualTenderActivity = new ActualTenderActivity();
    oldActualTenderActivity.setId(id);
    oldActualTenderActivity.setActualTender(oldActualTender);
    oldActualTenderActivity.setContractStage(ContractStage.INVITATION_TO_TENDER_IS_LIVE);
    oldActualTenderActivity.setRemunerationModel(RemunerationModel.REIMBURSABLE);
    oldActualTenderActivity.setScopeTitle("Test");
    oldActualTenderActivity.setScopeDescription("Test Test");
    return oldActualTenderActivity;
  }
}
