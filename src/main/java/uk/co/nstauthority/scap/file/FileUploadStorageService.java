package uk.co.nstauthority.scap.file;

import com.amazonaws.services.s3.model.ObjectMetadata;
import java.io.InputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import uk.co.nstauthority.scap.file.s3.S3ClientService;

@Service
class FileUploadStorageService {
  private static final Logger LOGGER = LoggerFactory.getLogger(FileUploadStorageService.class);

  private final S3ClientService s3ClientService;

  @Autowired
  FileUploadStorageService(S3ClientService s3ClientService) {
    this.s3ClientService = s3ClientService;
  }

  void uploadFile(MultipartFile multipartFile, UploadedFile uploadedFile) {
    var objectMetadata = new ObjectMetadata();
    objectMetadata.setContentLength(uploadedFile.getFileSizeBytes());
    objectMetadata.setContentType(uploadedFile.getFileContentType());

    try {
      s3ClientService.storeFile(s3ClientService.getDefaultBucketName(), uploadedFile.getS3Key(), multipartFile.getInputStream(),
          objectMetadata);
      LOGGER.debug("Completed upload of file {}", uploadedFile.getFilename());
    } catch (Exception e) {
      throw new FileUploadException(multipartFile, e);
    }
  }

  void deleteFile(UploadedFile uploadedFile) {
    s3ClientService.deleteFile(uploadedFile.getBucketName(), uploadedFile.getS3Key());
  }

  InputStream downloadFile(UploadedFile uploadedFile) {
    return s3ClientService.fetchFile(uploadedFile.getBucketName(), uploadedFile.getS3Key());
  }
}
