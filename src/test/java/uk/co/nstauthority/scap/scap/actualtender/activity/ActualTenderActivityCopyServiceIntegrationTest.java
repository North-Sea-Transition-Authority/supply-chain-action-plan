package uk.co.nstauthority.scap.scap.actualtender.activity;

import static org.assertj.core.api.Assertions.assertThat;

import jakarta.persistence.EntityManager;
import jakarta.transaction.Transactional;
import java.time.Instant;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import uk.co.nstauthority.scap.integrationtest.AbstractIntegrationTest;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;

@Transactional
class ActualTenderActivityCopyServiceIntegrationTest extends AbstractIntegrationTest {

  @Autowired
  EntityManager entityManager;

  @Autowired
  ActualTenderActivityCopyService actualTenderActivityCopyService;

  // This test caters for https://fivium.atlassian.net/browse/EDU-6801. When multiple manual invitation tender participants were
  // added, it caused a crash when trying to update the SCAP, due to multiple results being returned from the database when
  // only one was expected.
  @Test
  void copyAwardedContracts_whenMultipleCompaniesWithNullOrgIds_thenPersistAwardedContract() {
    var scap = new Scap(55, Instant.now(), "TEST-SCAP-REF/1");
    var scapDetail = new ScapDetail(scap, 2, ScapDetailStatus.DRAFT, Instant.now(), 1);

    var actualTender = new ActualTender(scapDetail, Instant.now());
    actualTender.setHasActualTenders(true);
    var actualTenderActivity = createActualTenderActivity(actualTender);

    var preferredBidder1 = getPreferredBidder(actualTenderActivity, "company 1");
    var preferredBidder2 = getPreferredBidder(actualTenderActivity, "company 2");

    var awardedContract = new AwardedContract();
    awardedContract.setActualTenderActivity(actualTenderActivity);
    awardedContract.setPreferredBidder(preferredBidder1);

    entityManager.persist(scap);
    entityManager.persist(scapDetail);
    entityManager.persist(actualTender);
    entityManager.persist(actualTenderActivity);
    entityManager.persist(preferredBidder1);
    entityManager.persist(preferredBidder2);
    entityManager.persist(awardedContract);

    entityManager.flush();

    var resultsList = entityManager.createQuery("SELECT awardedContract from AwardedContract as awardedContract").getResultList();
    assertThat(resultsList).hasSize(1);

    actualTenderActivityCopyService.copyAwardedContracts(Map.of(actualTenderActivity, actualTenderActivity));

    resultsList = entityManager.createQuery("SELECT awardedContract from AwardedContract as awardedContract").getResultList();
    assertThat(resultsList).hasSize(2);

    AwardedContract originalAwardedContract = (AwardedContract) resultsList.get(0);
    AwardedContract copiedAwardedContract = (AwardedContract) resultsList.get(1);

    assertThat(originalAwardedContract)
        .extracting(
            AwardedContract::getPreferredBidder,
            AwardedContract::getActualTenderActivity
        )
        .containsExactly(copiedAwardedContract.getPreferredBidder(), copiedAwardedContract.getActualTenderActivity());
  }

  private ActualTenderActivity createActualTenderActivity(ActualTender actualTender) {
    var actualTenderActivity = new ActualTenderActivity(actualTender, Instant.now());
    actualTenderActivity.setScopeTitle("title");
    actualTenderActivity.setScopeDescription("description");
    actualTenderActivity.setRemunerationModel(RemunerationModel.LUMP_SUM);
    actualTenderActivity.setContractStage(ContractStage.CONTRACT_AWARDED);
    return actualTenderActivity;
  }

  private InvitationToTenderParticipant getPreferredBidder(ActualTenderActivity actualTenderActivity, String companyName) {
    var preferredBidder = new InvitationToTenderParticipant();
    preferredBidder.setActualTenderActivity(actualTenderActivity);
    preferredBidder.setCompanyName(companyName);
    preferredBidder.setOrganisationUnitId(null);
    return preferredBidder;
  }
}