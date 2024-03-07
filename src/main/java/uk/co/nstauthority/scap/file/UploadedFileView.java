package uk.co.nstauthority.scap.file;

import java.time.Instant;
import uk.co.nstauthority.scap.fds.fileupload.FileUploadItem;

public record UploadedFileView(String fileId, String fileName, String fileSize, String fileDescription,
                               Instant fileUploadedTime) implements FileUploadItem {
  // we need these getters as FDS' file-upload macro expects the getters to be named in this specific way
  @Override
  public String getFileId() {
    return fileId;
  }

  @Override
  public String getFileName() {
    return fileName;
  }

  @Override
  public String getFileSize() {
    return fileSize;
  }

  @Override
  public String getFileDescription() {
    return fileDescription;
  }

  @Override
  public Instant getFileUploadedTime() {
    return fileUploadedTime;
  }
}
