package uk.co.nstauthority.scap.file;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import java.time.Instant;
import java.util.UUID;
import org.hibernate.annotations.UuidGenerator;

@Entity
@Table(name = "uploaded_files")
public class UploadedFile {
  @Id
  @UuidGenerator
  private UUID id;

  @Column(name = "s3_key")
  private String s3Key;

  private String bucketName;

  private VirtualFolder virtualFolder;

  private String filename;

  private String fileContentType;

  private long fileSizeBytes;

  private Instant uploadedTimeStamp;

  private String description;

  public UUID getId() {
    return id;
  }

  public void setId(UUID id) {
    this.id = id;
  }

  public String getS3Key() {
    return s3Key;
  }

  public void setS3Key(String s3Key) {
    this.s3Key = s3Key;
  }

  public String getBucketName() {
    return bucketName;
  }

  public void setBucketName(String bucketName) {
    this.bucketName = bucketName;
  }

  public VirtualFolder getVirtualFolder() {
    return virtualFolder;
  }

  public void setVirtualFolder(VirtualFolder virtualFolder) {
    this.virtualFolder = virtualFolder;
  }

  public String getFilename() {
    return filename;
  }

  public void setFilename(String filename) {
    this.filename = filename;
  }

  public String getFileContentType() {
    return fileContentType;
  }

  public void setFileContentType(String fileContentType) {
    this.fileContentType = fileContentType;
  }

  public long getFileSizeBytes() {
    return fileSizeBytes;
  }

  public void setFileSizeBytes(long fileSizeBytes) {
    this.fileSizeBytes = fileSizeBytes;
  }

  public Instant getUploadedTimeStamp() {
    return uploadedTimeStamp;
  }

  public void setUploadedTimeStamp(Instant uploadedTimeStamp) {
    this.uploadedTimeStamp = uploadedTimeStamp;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }
}
