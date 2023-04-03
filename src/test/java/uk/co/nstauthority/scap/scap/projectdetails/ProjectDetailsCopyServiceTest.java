package uk.co.nstauthority.scap.scap.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static uk.co.nstauthority.scap.utils.ObjectTestingUtil.assertValuesEqual;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import javax.persistence.EntityManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentRepository;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsCopyServiceTest {

  EntityManager entityManager = mock(EntityManager.class);

  EntityCopyService entityCopyService = new EntityCopyService(entityManager);

  ProjectDetailsService projectDetailsService = mock(ProjectDetailsService.class);

  SupportingDocumentRepository supportingDocumentRepository = mock(SupportingDocumentRepository.class);

  ProjectDetailsCopyService projectDetailsCopyService = new ProjectDetailsCopyService(
      projectDetailsService,
      entityCopyService,
      supportingDocumentRepository, entityManager);

  @Captor
  ArgumentCaptor<ProjectDetails> projectDetailsCaptor;

  @Test
  void copyService_copyChild_copyProjectDetails() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldProjectDetails = new ProjectDetails();
    oldProjectDetails.setProjectName("Test");
    oldProjectDetails.setProjectCostEstimate(new BigDecimal(5000));
    oldProjectDetails.setEstimatedValueLocalContent(new BigDecimal(4000));
    oldProjectDetails.setHasFacilities(true);
    oldProjectDetails.setPlannedExecutionStartDate(LocalDate.now());
    oldProjectDetails.setPlannedCompletionDate(LocalDate.now());

    when(projectDetailsService.getByScapDetail(oldScapDetail)).thenReturn(oldProjectDetails);
    projectDetailsCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);

    verify(entityManager).persist(projectDetailsCaptor.capture());
    var result = projectDetailsCaptor.getValue();
    assertValuesEqual(result, oldProjectDetails, List.of("id", "scapDetail", "createdTimestamp"));
    assertThat(result.getScapDetail()).isEqualTo(newScapDetail);
    assertThat(result.getId()).isNull();
  }

  @Test
  void copyService_ProjectDetails_verifyChildrenCalls() {
    var oldScapDetail = new ScapDetail();
    var newScapDetail = new ScapDetail();

    var oldProjectDetails = new ProjectDetails();
    when(projectDetailsService.getByScapDetail(oldScapDetail)).thenReturn(oldProjectDetails);
    projectDetailsCopyService.copyEntity(oldScapDetail, newScapDetail, NewScapType.REINSTATEMENT);

    verify(entityManager).persist(projectDetailsCaptor.capture());
    var result = projectDetailsCaptor.getValue();

    verify(projectDetailsService).getProjectFields(oldProjectDetails);
    verify(projectDetailsService).getProjectFacilities(oldProjectDetails);
    verify(projectDetailsService).getProjectTypeEntitiesByProjectDetails(oldProjectDetails);
  }
}
