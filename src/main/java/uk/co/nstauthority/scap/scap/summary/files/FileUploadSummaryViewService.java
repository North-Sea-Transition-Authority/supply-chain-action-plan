package uk.co.nstauthority.scap.scap.summary.files;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentsController;

@Service
public class FileUploadSummaryViewService {

  private final SupportingDocumentService supportingDocumentService;
  private final FileUploadService fileUploadService;

  @Autowired
  FileUploadSummaryViewService(SupportingDocumentService supportingDocumentService, FileUploadService fileUploadService) {
    this.supportingDocumentService = supportingDocumentService;
    this.fileUploadService = fileUploadService;
  }

  public List<FileUploadSummaryView> getAllByScapDetailAndDocumentType(ScapDetail scapDetail, SupportingDocumentType type) {
    var fileUploadList = supportingDocumentService.getFileUploadFormListForScapDetailAndType(scapDetail, type);

    return fileUploadList
        .stream()
        .map(fileUpload -> fileUploadService.findUploadedFileOrThrow(fileUpload.getUploadedFileId()))
        .map(uploadedFile -> new FileUploadSummaryView(
            uploadedFile.getFilename(),
            uploadedFile.getDescription(),
            ReverseRouter.route(on(SupportingDocumentsController.class)
                .download(scapDetail.getScap().getScapId(), uploadedFile.getId()))))
        .toList();
  }
}
