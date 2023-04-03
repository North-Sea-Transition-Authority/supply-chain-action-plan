package uk.co.nstauthority.scap.scap.projectdetails;

import javax.persistence.EntityManager;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.copy.CopyService;
import uk.co.nstauthority.scap.scap.copy.EntityCopyService;
import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentRepository;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;

@Service
public class ProjectDetailsCopyService implements CopyService {

  private final ProjectDetailsService projectDetailsService;

  private final EntityCopyService entityCopyService;

  private final SupportingDocumentRepository supportingDocumentRepository;

  private final EntityManager entityManager;

  public ProjectDetailsCopyService(ProjectDetailsService projectDetailsService,
                                   EntityCopyService entityCopyService,
                                   SupportingDocumentRepository supportingDocumentRepository,
                                   EntityManager entityManager) {
    this.projectDetailsService = projectDetailsService;
    this.entityCopyService = entityCopyService;
    this.supportingDocumentRepository = supportingDocumentRepository;
    this.entityManager = entityManager;
  }

  @Override
  public int runOrder() {
    return 1;
  }

  @Override
  public void copyEntity(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType newScapType) {
    var oldProjectDetails = projectDetailsService.getByScapDetail(oldScapDetail);
    var newProjectDetails = (ProjectDetails) entityCopyService.copyChild(newScapDetail, oldProjectDetails);

    copyOverFields(oldProjectDetails, newProjectDetails);
    copyOverProjectType(oldProjectDetails, newProjectDetails);
    copyOverFacilities(oldProjectDetails, newProjectDetails);
    copyOverSupportingDocuments(oldScapDetail, newScapDetail);
  }

  private void copyOverFields(ProjectDetails oldProjectdetails, ProjectDetails newProjectDetails) {
    var oldFields = projectDetailsService.getProjectFields(oldProjectdetails);
    entityCopyService.copyChildren(newProjectDetails, oldFields);
  }

  private void copyOverFacilities(ProjectDetails oldProjectDetails, ProjectDetails newProjectDetails) {
    var oldFacilities = projectDetailsService.getProjectFacilities(oldProjectDetails);
    entityCopyService.copyChildren(newProjectDetails, oldFacilities);
  }

  private void copyOverProjectType(ProjectDetails oldProjectDetails, ProjectDetails newProjectDetails) {
    var oldProjectTypes = projectDetailsService.getProjectTypeEntitiesByProjectDetails(oldProjectDetails);
    entityCopyService.copyChildren(newProjectDetails, oldProjectTypes);
  }

  private void copyOverSupportingDocuments(ScapDetail oldScapDetail, ScapDetail newScapDetail) {
    supportingDocumentRepository
        .findAllByScapDetailAndSupportingDocumentType(oldScapDetail, SupportingDocumentType.ADDITIONAL_DOCUMENT)
        .forEach(supportingDocument -> {
          entityManager.detach(supportingDocument);
          supportingDocument.setId(null);
          supportingDocument.setScapDetail(newScapDetail);
          entityManager.persist(supportingDocument);
        });
  }
}
