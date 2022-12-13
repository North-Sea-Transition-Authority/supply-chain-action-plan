package uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments;

import java.util.List;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

interface SupportingDocumentRepository extends CrudRepository<SupportingDocument, UUID> {

  List<SupportingDocument> findAllByScapDetailAndSupportingDocumentType(
      ScapDetail scapDetail, SupportingDocumentType supportingDocumentType);

  SupportingDocument findByScapDetailAndUploadedFile(ScapDetail scapDetail, UploadedFile uploadedFile);

  List<SupportingDocument> findAllByUploadedFile(UploadedFile uploadedFile);
}