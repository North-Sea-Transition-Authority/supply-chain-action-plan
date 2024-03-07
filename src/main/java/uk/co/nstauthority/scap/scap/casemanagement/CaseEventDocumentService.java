package uk.co.nstauthority.scap.scap.casemanagement;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import uk.co.nstauthority.scap.file.FileDeleteResult;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.file.FileUploadResult;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.file.FileUploadValidationService;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.file.VirtualFolder;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Service
public class CaseEventDocumentService {
  private final FileUploadService fileUploadService;
  private final FileUploadValidationService fileUploadValidationService;

  @Autowired
  public CaseEventDocumentService(FileUploadService fileUploadService,
                                  FileUploadValidationService fileUploadValidationService) {
    this.fileUploadService = fileUploadService;
    this.fileUploadValidationService = fileUploadValidationService;
  }

  @Transactional
  public FileUploadResult processFileUpload(MultipartFile multipartFile) {
    var fileSize = multipartFile.getSize();
    var filename = fileUploadService.sanitiseFilename(Objects.requireNonNull(multipartFile.getOriginalFilename()));
    var contentType = multipartFile.getContentType();

    var uploadErrorType = fileUploadValidationService.validateFileUpload(multipartFile, fileSize, filename);
    if (uploadErrorType != null) {
      return FileUploadResult.error(filename, multipartFile, uploadErrorType);
    }

    var uploadedFile = fileUploadService.createUploadedFile(
        VirtualFolder.SUPPORTING_DOCUMENTS, fileSize, filename, contentType);

    // Persist uploaded file and supporting document records first,
    // so if it fails, we don't upload. If upload fails, we will roll back
    fileUploadService.uploadFile(multipartFile, uploadedFile);

    return FileUploadResult.valid(uploadedFile.getId().toString(), filename, multipartFile);
  }

  public FileUploadTemplate buildFileUploadTemplate(ScapId scapId, SupportingDocumentType supportingDocumentType) {
    return fileUploadService.buildFileUploadTemplate(
        ReverseRouter.route(on(
            CaseEventsDocumentController.class).download(scapId, null)),
        ReverseRouter.route(on(
            CaseEventsDocumentController.class).upload(scapId, supportingDocumentType, null)),
        ReverseRouter.route(on(
            CaseEventsDocumentController.class).delete(scapId, null))
    );
  }

  @Transactional
  public FileDeleteResult deleteFile(UUID uploadedFileId) {
    var uploadedFile = fileUploadService.getUploadedFile(uploadedFileId);
    fileUploadService.deleteFile(uploadedFile);

    return FileDeleteResult.success(uploadedFile.getId().toString());
  }

  @Transactional
  public void updateSupportingDocumentFileDescriptions(List<FileUploadForm> fileUploadFormList) {
    fileUploadService.updateFileUploadDescriptions(fileUploadFormList);
  }

  public UploadedFile getUploadedFile(UUID uploadedFileId) {
    return fileUploadService.getUploadedFile(uploadedFileId);
  }
}
