package uk.co.nstauthority.scap.scap.summary.contractingperformance;

import java.util.List;
import java.util.Optional;
import javax.persistence.EntityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;

@Repository
class ContractingPerformanceSummaryDtoRepository {

  private final EntityManager entityManager;

  @Autowired
  ContractingPerformanceSummaryDtoRepository(EntityManager entityManager) {
    this.entityManager = entityManager;
  }

  List<ContractingPerformanceSummaryDto> getAllByScapId(Integer scapId) {
    return getAllByScapIdAndContractingPerformanceId(scapId, null);
  }

  Optional<ContractingPerformanceSummaryDto> findByScapIdAndContractingPerformanceId(Integer scapId,
                                                                                     Integer contractingPerformanceId) {
    return getAllByScapIdAndContractingPerformanceId(scapId, contractingPerformanceId).stream().findFirst();
  }

  private List<ContractingPerformanceSummaryDto> getAllByScapIdAndContractingPerformanceId(Integer scapId,
                                                                                           Integer contractingPerformanceId) {
    return entityManager.createQuery(
        "SELECT new uk.co.nstauthority.scap.scap.summary.contractingperformance.ContractingPerformanceSummaryDto(" +
            "cp.id, ata.scopeTitle, ata.scopeDescription, ac.awardValue, ata.remunerationModel, " +
            "ata.remunerationModelName, " +
            "ittp.companyName, ac.preferredBidderCountryId, cp.outturnCost, cp.outturnRationale) " +
            "FROM Scap s " +
            "JOIN ScapDetail sd ON sd.scap = s AND sd.tipFlag = true " +
            "JOIN ContractingPerformanceOverview cpo ON cpo.scapDetail = sd " +
            "JOIN ContractingPerformance cp ON cp.contractingPerformanceOverview = cpo " +
            "JOIN ActualTenderActivity ata ON cp.actualTenderActivity = ata " +
            "JOIN AwardedContract ac ON ac.actualTenderActivity = ata " +
            "JOIN InvitationToTenderParticipant ittp ON ac.preferredBidder = ittp " +
            "WHERE s.id = :scapId " +
            "AND :contractingPerformanceId IS NULL OR cp.id = :contractingPerformanceId", ContractingPerformanceSummaryDto.class)
        .setParameter("scapId", scapId)
        .setParameter("contractingPerformanceId", contractingPerformanceId)
        .getResultList();
  }
}
