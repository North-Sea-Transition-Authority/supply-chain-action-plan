package uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments;

import java.util.UUID;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import org.hibernate.annotations.GenericGenerator;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@Entity
@Table(name = "supporting_documents")
public class SupportingDocument {

  @Id
  @GeneratedValue(generator = "uuid")
  @GenericGenerator(name = "uuid", strategy = "uuid2")
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
