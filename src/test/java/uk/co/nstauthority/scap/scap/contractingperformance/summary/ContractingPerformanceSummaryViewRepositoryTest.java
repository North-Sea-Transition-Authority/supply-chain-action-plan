package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;

import java.math.BigDecimal;
import java.time.Instant;
import javax.transaction.Transactional;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.TestEntityManager;
import uk.co.nstauthority.scap.IntegrationTest;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformance;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;

@Transactional
@IntegrationTest
class ContractingPerformanceSummaryViewRepositoryTest {

  @Autowired
  TestEntityManager entityManager;

  @Autowired
  ContractingPerformanceSummaryViewRepository contractingPerformanceSummaryViewRepository;

  @Test
  void getContractingPerformanceSummaryViews_AssertCorrectResult() {
    var scap = new Scap(59803, Instant.now());
    var otherScap = new Scap(4598, Instant.now());

    entityManager.persistAndFlush(scap);
    entityManager.persistAndFlush(otherScap);

    var scapDetail = new ScapDetail(scap, 1, true, ScapDetailStatus.DRAFT, Instant.now(), 0);
    var otherScapDetail = new ScapDetail(otherScap, 1, true, ScapDetailStatus.DRAFT, Instant.now(), 0);

    entityManager.persistAndFlush(scapDetail);
    entityManager.persistAndFlush(otherScapDetail);

    var contractingPerformanceOverview = new ContractingPerformanceOverview(scapDetail, Instant.now());
    var otherContractingPerformanceOverview = new ContractingPerformanceOverview(otherScapDetail, Instant.now());

    entityManager.persistAndFlush(contractingPerformanceOverview);
    entityManager.persistAndFlush(otherContractingPerformanceOverview);

    var actualTenderActivity1 = new ActualTenderActivity();
    actualTenderActivity1.setScopeTitle("scope title 1");
    actualTenderActivity1.setScopeDescription("some scope desc 1");
    actualTenderActivity1.setRemunerationModel(RemunerationModel.OTHER);
    actualTenderActivity1.setRemunerationModelName("some remuneration model name");
    var actualTenderActivity2 = new ActualTenderActivity();
    actualTenderActivity2.setScopeTitle("scope title 2");
    actualTenderActivity2.setScopeDescription("some scope desc 2");
    actualTenderActivity2.setRemunerationModel(RemunerationModel.LUMP_SUM);
    var otherActualTenderActivity = new ActualTenderActivity();

    entityManager.persistAndFlush(actualTenderActivity1);
    entityManager.persistAndFlush(actualTenderActivity2);
    entityManager.persistAndFlush(otherActualTenderActivity);

    var contractor1 = new InvitationToTenderParticipant(actualTenderActivity1, Instant.now());
    contractor1.setCompanyName("company name 1");
    var contractor2 = new InvitationToTenderParticipant(actualTenderActivity2, Instant.now());
    contractor2.setCompanyName("company name 2");
    var otherContractor = new InvitationToTenderParticipant(otherActualTenderActivity, Instant.now());

    entityManager.persistAndFlush(contractor1);
    entityManager.persistAndFlush(contractor2);
    entityManager.persistAndFlush(otherContractor);

    var awardedContract1 = new AwardedContract(actualTenderActivity1, Instant.now());
    awardedContract1.setAwardValue(BigDecimal.valueOf(58.03));
    awardedContract1.setPreferredBidderLocation(0);
    awardedContract1.setPreferredBidder(contractor1);
    var awardedContract2 = new AwardedContract(actualTenderActivity2, Instant.now());
    awardedContract2.setAwardValue(BigDecimal.valueOf(58.31));
    awardedContract2.setPreferredBidderLocation(1);
    awardedContract2.setPreferredBidder(contractor2);
    var otherAwardedContract = new AwardedContract(otherActualTenderActivity, Instant.now());

    entityManager.persistAndFlush(awardedContract1);
    entityManager.persistAndFlush(awardedContract2);
    entityManager.persistAndFlush(otherAwardedContract);

    var contractingPerformance1 = new ContractingPerformance(contractingPerformanceOverview, Instant.now());
    contractingPerformance1.setActualTenderActivity(actualTenderActivity1);
    contractingPerformance1.setOutturnCost(BigDecimal.valueOf(54.32));
    contractingPerformance1.setOutturnRationale("some outturn rationale 1");
    var contractingPerformance2 = new ContractingPerformance(contractingPerformanceOverview, Instant.now());
    contractingPerformance2.setActualTenderActivity(actualTenderActivity2);
    contractingPerformance2.setOutturnCost(BigDecimal.valueOf(59.13));
    contractingPerformance2.setOutturnRationale("some outturn rationale 2");
    var otherContractingPerformance = new ContractingPerformance(otherContractingPerformanceOverview, Instant.now());

    entityManager.persistAndFlush(contractingPerformance1);
    entityManager.persistAndFlush(contractingPerformance2);
    entityManager.persistAndFlush(otherContractingPerformance);

    var summaryViews = contractingPerformanceSummaryViewRepository
        .getContractingPerformanceSummaryViewsByScapId(scap.getId());

    assertThat(summaryViews).extracting(
        ContractingPerformanceSummaryView::contractingPerformanceId,
        ContractingPerformanceSummaryView::scopeTitle,
        ContractingPerformanceSummaryView::scopeDescription,
        ContractingPerformanceSummaryView::awardValue,
        ContractingPerformanceSummaryView::remunerationModel,
        ContractingPerformanceSummaryView::remunerationModelName,
        ContractingPerformanceSummaryView::contractor,
        ContractingPerformanceSummaryView::countryId,
        ContractingPerformanceSummaryView::outturnCost,
        ContractingPerformanceSummaryView::outturnRationale
    ).containsExactly(
        tuple(
            contractingPerformance1.getId(),
            actualTenderActivity1.getScopeTitle(),
            actualTenderActivity1.getScopeDescription(),
            awardedContract1.getAwardValue(),
            actualTenderActivity1.getRemunerationModel(),
            actualTenderActivity1.getRemunerationModelName(),
            contractor1.getCompanyName(),
            awardedContract1.getPreferredBidderLocation(),
            contractingPerformance1.getOutturnCost(),
            contractingPerformance1.getOutturnRationale()
        ),
        tuple(
            contractingPerformance2.getId(),
            actualTenderActivity2.getScopeTitle(),
            actualTenderActivity2.getScopeDescription(),
            awardedContract2.getAwardValue(),
            actualTenderActivity2.getRemunerationModel(),
            actualTenderActivity2.getRemunerationModelName(),
            contractor2.getCompanyName(),
            awardedContract2.getPreferredBidderLocation(),
            contractingPerformance2.getOutturnCost(),
            contractingPerformance2.getOutturnRationale()
        )
    );

  }


}
