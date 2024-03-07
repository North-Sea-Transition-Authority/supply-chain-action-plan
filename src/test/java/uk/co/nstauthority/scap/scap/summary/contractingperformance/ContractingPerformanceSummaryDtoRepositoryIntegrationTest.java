package uk.co.nstauthority.scap.scap.summary.contractingperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;

import java.math.BigDecimal;
import java.time.Instant;
import javax.persistence.EntityManager;
import javax.transaction.Transactional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import uk.co.nstauthority.scap.integrationtest.AbstractIntegrationTest;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformance;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.Scap;

@Transactional
class ContractingPerformanceSummaryDtoRepositoryIntegrationTest extends AbstractIntegrationTest {

  @Autowired
  EntityManager entityManager;

  @Autowired
  ContractingPerformanceSummaryDtoRepository contractingPerformanceSummaryDtoRepository;

  private Scap scap;
  private ActualTenderActivity actualTenderActivity1;
  private ActualTenderActivity actualTenderActivity2;
  private InvitationToTenderParticipant contractor1;
  private InvitationToTenderParticipant contractor2;
  private AwardedContract awardedContract1;
  private AwardedContract awardedContract2;
  private ContractingPerformance contractingPerformance1;
  private ContractingPerformance contractingPerformance2;

  @BeforeEach
  void setup() {
    scap = new Scap(59803, Instant.now(), null);
    scap.setReference("SCAP/2023/1");
    var otherScap = new Scap(4598, Instant.now(), null);
    otherScap.setReference("SCAP/2023/2");

    entityManager.persist(scap);
    entityManager.persist(otherScap);

    var scapDetail = new ScapDetail(scap, 1, ScapDetailStatus.DRAFT, Instant.now(), 0);
    var otherScapDetail = new ScapDetail(otherScap, 1, ScapDetailStatus.DRAFT, Instant.now(), 0);

    entityManager.persist(scapDetail);
    entityManager.persist(otherScapDetail);

    ContractingPerformanceOverview contractingPerformanceOverview = new ContractingPerformanceOverview(scapDetail,
        Instant.now());
    ContractingPerformanceOverview otherContractingPerformanceOverview = new ContractingPerformanceOverview(
        otherScapDetail, Instant.now());

    entityManager.persist(contractingPerformanceOverview);
    entityManager.persist(otherContractingPerformanceOverview);

    var actualTender = new ActualTender(scapDetail, Instant.now());
    var otherActualTender = new ActualTender(otherScapDetail, Instant.now());

    entityManager.persist(actualTender);
    entityManager.persist(otherActualTender);

    actualTenderActivity1 = new ActualTenderActivity(actualTender, Instant.now());
    actualTenderActivity1.setScopeTitle("scope title 1");
    actualTenderActivity1.setScopeDescription("some scope desc 1");
    actualTenderActivity1.setRemunerationModel(RemunerationModel.OTHER);
    actualTenderActivity1.setRemunerationModelName("some remuneration model name");
    actualTenderActivity2 = new ActualTenderActivity(actualTender, Instant.now());
    actualTenderActivity2.setScopeTitle("scope title 2");
    actualTenderActivity2.setScopeDescription("some scope desc 2");
    actualTenderActivity2.setRemunerationModel(RemunerationModel.LUMP_SUM);
    var otherActualTenderActivity = new ActualTenderActivity(otherActualTender, Instant.now());

    entityManager.persist(actualTenderActivity1);
    entityManager.persist(actualTenderActivity2);
    entityManager.persist(otherActualTenderActivity);

    contractor1 = new InvitationToTenderParticipant(actualTenderActivity1, Instant.now(), "company name 1");
    contractor2 = new InvitationToTenderParticipant(actualTenderActivity2, Instant.now(), "company name 2");
    var otherContractor = new InvitationToTenderParticipant(otherActualTenderActivity,
        Instant.now(), "other company name");

    entityManager.persist(contractor1);
    entityManager.persist(contractor2);
    entityManager.persist(otherContractor);

    awardedContract1 = new AwardedContract(actualTenderActivity1, Instant.now());
    awardedContract1.setAwardValue(BigDecimal.valueOf(58.03));
    awardedContract1.setPreferredBidderCountryId(0);
    awardedContract1.setPreferredBidder(contractor1);
    awardedContract2 = new AwardedContract(actualTenderActivity2, Instant.now());
    awardedContract2.setAwardValue(BigDecimal.valueOf(58.31));
    awardedContract2.setPreferredBidderCountryId(1);
    awardedContract2.setPreferredBidder(contractor2);
    var otherAwardedContract = new AwardedContract(otherActualTenderActivity, Instant.now());

    entityManager.persist(awardedContract1);
    entityManager.persist(awardedContract2);
    entityManager.persist(otherAwardedContract);

    contractingPerformance1 = new ContractingPerformance(contractingPerformanceOverview, Instant.now());
    contractingPerformance1.setActualTenderActivity(actualTenderActivity1);
    contractingPerformance1.setOutturnCost(BigDecimal.valueOf(54.32));
    contractingPerformance1.setOutturnRationale("some outturn rationale 1");
    contractingPerformance2 = new ContractingPerformance(contractingPerformanceOverview, Instant.now());
    contractingPerformance2.setActualTenderActivity(actualTenderActivity2);
    contractingPerformance2.setOutturnCost(BigDecimal.valueOf(59.13));
    contractingPerformance2.setOutturnRationale("some outturn rationale 2");
    var otherContractingPerformance = new ContractingPerformance(otherContractingPerformanceOverview, Instant.now());

    entityManager.persist(contractingPerformance1);
    entityManager.persist(contractingPerformance2);
    entityManager.persist(otherContractingPerformance);

    entityManager.flush();
  }

  @Test
  void getAllByScapId() {
    var summaryViews = contractingPerformanceSummaryDtoRepository
        .getAllByScapId(scap.getScapId());

    assertThat(summaryViews).extracting(
        ContractingPerformanceSummaryDto::contractingPerformanceId,
        ContractingPerformanceSummaryDto::scopeTitle,
        ContractingPerformanceSummaryDto::scopeDescription,
        ContractingPerformanceSummaryDto::awardValue,
        ContractingPerformanceSummaryDto::remunerationModel,
        ContractingPerformanceSummaryDto::remunerationModelName,
        ContractingPerformanceSummaryDto::contractor,
        ContractingPerformanceSummaryDto::countryId,
        ContractingPerformanceSummaryDto::outturnCost,
        ContractingPerformanceSummaryDto::outturnRationale
    ).containsExactly(
        tuple(
            contractingPerformance1.getId(),
            actualTenderActivity1.getScopeTitle(),
            actualTenderActivity1.getScopeDescription(),
            awardedContract1.getAwardValue(),
            actualTenderActivity1.getRemunerationModel(),
            actualTenderActivity1.getRemunerationModelName(),
            contractor1.getCompanyName(),
            awardedContract1.getPreferredBidderCountryId(),
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
            awardedContract2.getPreferredBidderCountryId(),
            contractingPerformance2.getOutturnCost(),
            contractingPerformance2.getOutturnRationale()
        )
    );

  }

  @Test
  void findByScapIdAndContractingPerformanceId() {
    var summaryView = contractingPerformanceSummaryDtoRepository
        .findByScapIdAndContractingPerformanceId(scap.getScapId(), contractingPerformance1.getId());

    assertThat(summaryView).isNotEmpty();
    assertThat(summaryView.get()).extracting(
        ContractingPerformanceSummaryDto::contractingPerformanceId,
        ContractingPerformanceSummaryDto::scopeTitle,
        ContractingPerformanceSummaryDto::scopeDescription,
        ContractingPerformanceSummaryDto::awardValue,
        ContractingPerformanceSummaryDto::remunerationModel,
        ContractingPerformanceSummaryDto::remunerationModelName,
        ContractingPerformanceSummaryDto::contractor,
        ContractingPerformanceSummaryDto::countryId,
        ContractingPerformanceSummaryDto::outturnCost,
        ContractingPerformanceSummaryDto::outturnRationale
    ).containsExactly(
        contractingPerformance1.getId(),
        actualTenderActivity1.getScopeTitle(),
        actualTenderActivity1.getScopeDescription(),
        awardedContract1.getAwardValue(),
        actualTenderActivity1.getRemunerationModel(),
        actualTenderActivity1.getRemunerationModelName(),
        contractor1.getCompanyName(),
        awardedContract1.getPreferredBidderCountryId(),
        contractingPerformance1.getOutturnCost(),
        contractingPerformance1.getOutturnRationale()
    );

  }


}
