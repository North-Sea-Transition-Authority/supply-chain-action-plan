package uk.co.nstauthority.scap.scap.contractingperformance.summary;

import java.util.List;
import javax.persistence.EntityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
class ContractingPerformanceSummaryViewRepository {

  private final EntityManager entityManager;

  @Autowired
  ContractingPerformanceSummaryViewRepository(EntityManager entityManager) {
    this.entityManager = entityManager;
  }

  List<ContractingPerformanceSummaryView> getContractingPerformanceSummaryViewsByScapId(Integer scapId) {
    return entityManager.createQuery(
        "SELECT new uk.co.nstauthority.scap.scap.contractingperformance.summary.ContractingPerformanceSummaryView(" +
            "s.id, cp.id, ata.scopeTitle, ata.scopeDescription, ac.awardValue, ata.remunerationModel, " +
            "ata.remunerationModelName, " +
            "ittp.companyName, ac.preferredBidderLocation, cp.outturnCost, cp.outturnRationale) " +
            "FROM Scap s " +
            "JOIN ScapDetail sd ON sd.scap = s AND sd.tipFlag = true " +
            "JOIN ContractingPerformanceOverview cpo ON cpo.scapDetail = sd " +
            "JOIN ContractingPerformance cp ON cp.contractingPerformanceOverview = cpo " +
            "JOIN ActualTenderActivity ata ON cp.actualTenderActivity = ata " +
            "JOIN AwardedContract ac ON ac.actualTenderActivity = ata " +
            "JOIN InvitationToTenderParticipant ittp ON ac.preferredBidder = ittp " +
            "WHERE s.id = :scapId ", ContractingPerformanceSummaryView.class)
        .setParameter("scapId", scapId)
        .getResultList();
  }
}
