package uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.file.FileDeleteResult;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.file.FileUploadResult;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.file.FileUploadValidationService;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.file.UploadedFileView;
import uk.co.nstauthority.scap.file.VirtualFolder;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@Service
public class SupportingDocumentService {
  private final FileUploadService fileUploadService;
  private final FileUploadValidationService fileUploadValidationService;
  private final SupportingDocumentRepository supportingDocumentRepository;

  @Autowired
  public SupportingDocumentService(FileUploadService fileUploadService,
                                   FileUploadValidationService fileUploadValidationService,
                                   SupportingDocumentRepository supportingDocumentRepository) {
    this.fileUploadService = fileUploadService;
    this.fileUploadValidationService = fileUploadValidationService;
    this.supportingDocumentRepository = supportingDocumentRepository;
  }

  @Transactional
  public FileUploadResult processFileUpload(ScapDetail scapDetail, SupportingDocumentType supportingDocumentType,
                                            MultipartFile multipartFile) {
    var fileSize = multipartFile.getSize();
    var filename = fileUploadService.sanitiseFilename(Objects.requireNonNull(multipartFile.getOriginalFilename()));
    var contentType = multipartFile.getContentType();

    var uploadErrorType = fileUploadValidationService.validateFileUpload(multipartFile, fileSize, filename);
    if (uploadErrorType != null) {
      return FileUploadResult.error(filename, multipartFile, uploadErrorType);
    }

    var uploadedFile = fileUploadService.createUploadedFile(
        VirtualFolder.SUPPORTING_DOCUMENTS, fileSize, filename, contentType);

    var supportingDocument = new SupportingDocument();
    supportingDocument.setScapDetail(scapDetail);
    supportingDocument.setSupportingDocumentType(supportingDocumentType);
    supportingDocument.setUploadedFile(uploadedFile);

    supportingDocumentRepository.save(supportingDocument);

    // Persist uploaded file and supporting document records first,
    // so if it fails, we don't upload. If upload fails, we will roll back
    fileUploadService.uploadFile(multipartFile, uploadedFile);

    return FileUploadResult.valid(uploadedFile.getId().toString(), filename, multipartFile);
  }

  public List<FileUploadForm> getFileUploadFormListForScapDetailAndType(ScapDetail scapDetail,
                                                                        SupportingDocumentType supportingDocumentType) {
    return supportingDocumentRepository
        .findAllByScapDetailAndSupportingDocumentType(scapDetail, supportingDocumentType).stream()
        .map(SupportingDocument::getUploadedFile)
        .map(fileUploadService::createFileUploadForm)
        .toList();
  }

  public List<UploadedFileView> getUploadedFileViewList(List<UUID> fileUploadIdList) {
    return fileUploadService.getUploadedFileViewList(fileUploadIdList);
  }

  public FileUploadTemplate buildFileUploadTemplate(ScapId scapDetailId, SupportingDocumentType supportingDocumentType) {
    return switch (supportingDocumentType) {
      case ADDITIONAL_DOCUMENT -> fileUploadService.buildFileUploadTemplate(
          ReverseRouter.route(on(AdditionalDocumentsController.class).download(scapDetailId, null)),
          ReverseRouter.route(
              on(AdditionalDocumentsController.class).upload(scapDetailId, null)),
          ReverseRouter.route(on(AdditionalDocumentsController.class).delete(scapDetailId, null))
      );
      case CONSULTATION_REPORT, APPROVAL_DOCUMENT -> fileUploadService.buildFileUploadTemplate(
          ReverseRouter.route(on(CaseEventsDocumentController.class).download(scapDetailId, null)),
          ReverseRouter.route(
              on(CaseEventsDocumentController.class).upload(scapDetailId, supportingDocumentType, null)),
          ReverseRouter.route(on(CaseEventsDocumentController.class).delete(scapDetailId, null))
      );
      default -> throw new ScapEntityNotFoundException("Could not find document management paths for document type");
    };
  }

  @Transactional
  public FileDeleteResult deleteFile(ScapDetail scapDetail, UUID uploadedFileId) {
    var uploadedFile = fileUploadService.findUploadedFileOrThrow(uploadedFileId);
    var supportingDocuments = supportingDocumentRepository.findAllByUploadedFile(uploadedFile);

    var supportingDocument =
        supportingDocumentRepository.findByScapDetailAndUploadedFile(scapDetail, uploadedFile);
    supportingDocumentRepository.delete(supportingDocument);

    // Check if the uploaded file is associated with other submission supporting documents and only delete if it is not
    if (supportingDocuments.size() == 1) {
      fileUploadService.deleteFile(uploadedFile);
    }
    // TODO SCAP2022-190 add logging for else size is greater than one

    return FileDeleteResult.success(uploadedFile.getId().toString());
  }

  @Transactional
  public void updateSupportingDocumentFileDescriptions(List<FileUploadForm> fileUploadFormList) {
    fileUploadService.updateFileUploadDescriptions(fileUploadFormList);
  }

  UploadedFile findUploadedFileOrThrow(ScapDetail scapDetail, UUID uploadedFileId) {
    var uploadedFile = fileUploadService.findUploadedFileOrThrow(uploadedFileId);
    var supportingDocument = supportingDocumentRepository.findByScapDetailAndUploadedFile(
        scapDetail, uploadedFile);
    return supportingDocument.getUploadedFile();
  }
}
