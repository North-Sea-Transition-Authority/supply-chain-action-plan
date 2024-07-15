package uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments;

import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import java.util.UUID;
import org.hibernate.annotations.UuidGenerator;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "supporting_documents")
public class SupportingDocument {

  @Id
  @UuidGenerator
  private UUID id;

  @ManyToOne
  @JoinColumn(name = "scap_detail_id")
  private ScapDetail scapDetail;

  @Enumerated(EnumType.STRING)
  private SupportingDocumentType supportingDocumentType;

  @ManyToOne
  @JoinColumn(name = "uploaded_file_id")
  private UploadedFile uploadedFile;

  public void setId(UUID id) {
    this.id = id;
  }

  public void setScapDetail(ScapDetail scapDetail) {
    this.scapDetail = scapDetail;
  }

  public void setSupportingDocumentType(SupportingDocumentType supportingDocumentType) {
    this.supportingDocumentType = supportingDocumentType;
  }

  public UploadedFile getUploadedFile() {
    return uploadedFile;
  }

  public void setUploadedFile(UploadedFile uploadedFile) {
    this.uploadedFile = uploadedFile;
  }
}
