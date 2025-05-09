package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;

import jakarta.persistence.EntityManager;
import jakarta.transaction.Transactional;
import java.time.Clock;
import java.util.Collections;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.springframework.beans.factory.annotation.Autowired;
import uk.co.nstauthority.scap.integrationtest.AbstractIntegrationTest;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverview;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetails;
import uk.co.nstauthority.scap.scap.projectperformance.ProjectPerformance;
import uk.co.nstauthority.scap.scap.scap.Scap;

@Transactional
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class WorkAreaItemDtoRepositoryIntegrationTest extends AbstractIntegrationTest {

  @Autowired
  EntityManager entityManager;

  @Autowired
  WorkAreaItemDtoRepository workAreaItemDtoRepository;

  @Autowired
  Clock clock;

  private Scap scap;
  private Scap otherOrganisationScap;
  private ScapDetail scapDetail;
  private ScapDetail otherScapDetail;
  private ProjectDetails projectDetails;
  private ProjectPerformance projectPerformance;
  private ContractingPerformanceOverview contractingPerformanceOverview;
  private ActualTender actualTender;
  private PlannedTender plannedTender;

  // Creating data used by tests and inserting it into the test database
  @BeforeEach
  void setup() {
    scap = new Scap(55, clock.instant(), "TEST-SCAP-REF/1");
    otherOrganisationScap = new Scap(1200, clock.instant(), "TEST-SCAP-REF/2");
    scapDetail = new ScapDetail(scap, 2, ScapDetailStatus.DRAFT, clock.instant(), 1);
    otherScapDetail = new ScapDetail(otherOrganisationScap, 4, ScapDetailStatus.SUBMITTED, clock.instant(), 1);
    projectDetails = new ProjectDetails(scapDetail, clock.instant());
    projectDetails.setProjectName("Test project name");
    projectPerformance = new ProjectPerformance(scapDetail, clock.instant());
    projectPerformance.setProjectCompleted(false);
    contractingPerformanceOverview = new ContractingPerformanceOverview(scapDetail, clock.instant());
    contractingPerformanceOverview.setHasContractingPerformance(false);
    actualTender = new ActualTender(scapDetail, clock.instant());
    actualTender.setHasActualTenders(true);
    plannedTender = new PlannedTender(scapDetail, clock.instant());
    plannedTender.setHasPlannedTenders(true);

    entityManager.persist(scap);
    entityManager.persist(otherOrganisationScap);
    entityManager.persist(scapDetail);
    entityManager.persist(otherScapDetail);
    entityManager.persist(projectDetails);
    entityManager.persist(projectPerformance);
    entityManager.persist(contractingPerformanceOverview);
    entityManager.persist(actualTender);
    entityManager.persist(plannedTender);

    entityManager.flush();
  }

  @Test
  void getAllByOrganisationGroups() {
    var workAreaItemDtoList = workAreaItemDtoRepository
        .getAllByOrganisationGroups(Collections.singletonList(scap.getOrganisationGroupId()));

    assertThat(workAreaItemDtoList).extracting(
        WorkAreaItemDto::scapId,
        WorkAreaItemDto::scapVersionNumber,
        WorkAreaItemDto::reference,
        WorkAreaItemDto::projectName,
        WorkAreaItemDto::organisationGroupId,
        WorkAreaItemDto::status,
        WorkAreaItemDto::projectClosedOut,
        WorkAreaItemDto::hasContractingPerformance,
        WorkAreaItemDto::hasActualTender,
        WorkAreaItemDto::hasPlannedTender
    ).containsExactly(tuple(
        scap.getId(),
        scapDetail.getVersionNumber(),
        scap.getReference(),
        projectDetails.getProjectName(),
        scap.getOrganisationGroupId(),
        scapDetail.getStatus(),
        projectPerformance.getProjectCompleted(),
        contractingPerformanceOverview.getHasContractingPerformance(),
        actualTender.getHasActualTenders(),
        plannedTender.getHasPlannedTenders()
    ));
  }

  @Test
  void getAllByScapStatusNotIn() {
    var workAreaItemDtoList = workAreaItemDtoRepository.getAllByScapStatusNotIn(ScapDetailStatus.DRAFT);

    assertThat(workAreaItemDtoList).extracting(
        WorkAreaItemDto::scapId,
        WorkAreaItemDto::scapVersionNumber,
        WorkAreaItemDto::reference,
        WorkAreaItemDto::projectName,
        WorkAreaItemDto::organisationGroupId,
        WorkAreaItemDto::status,
        WorkAreaItemDto::projectClosedOut,
        WorkAreaItemDto::hasContractingPerformance,
        WorkAreaItemDto::hasActualTender,
        WorkAreaItemDto::hasPlannedTender
    ).containsExactly(
        tuple(
            otherOrganisationScap.getId(),
            otherScapDetail.getVersionNumber(),
            otherOrganisationScap.getReference(),
            null,
            otherOrganisationScap.getOrganisationGroupId(),
            ScapDetailStatus.SUBMITTED,
            null,
            null,
            null,
            null
        )
    );
  }


}
