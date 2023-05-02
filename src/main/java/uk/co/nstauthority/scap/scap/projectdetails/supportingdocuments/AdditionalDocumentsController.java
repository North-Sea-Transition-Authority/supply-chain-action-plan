package uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments;

import java.util.UUID;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import uk.co.nstauthority.scap.endpointvalidation.annotations.HasAnyPermissionForScap;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.file.FileDeleteResult;
import uk.co.nstauthority.scap.file.FileUploadResult;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.FileUploadUtils;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@RestController
@RequestMapping("/{scapId}/additional-documents")
@HasAnyPermissionForScap(permissions = {RolePermission.SUBMIT_SCAP})
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class AdditionalDocumentsController {

  private final SupportingDocumentService supportingDocumentService;
  private final ScapDetailService scapDetailService;
  private final FileUploadService fileUploadService;

  @Autowired
  public AdditionalDocumentsController(SupportingDocumentService supportingDocumentService,
                                       ScapDetailService scapDetailService,
                                       FileUploadService fileUploadService) {
    this.supportingDocumentService = supportingDocumentService;
    this.scapDetailService = scapDetailService;
    this.fileUploadService = fileUploadService;
  }

  @PostMapping("upload")
  @ResponseBody
  public FileUploadResult upload(@PathVariable("scapId") ScapId scapId,
                                 @RequestParam("file") MultipartFile multipartFile) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);
    return supportingDocumentService.processFileUpload(scapDetail, SupportingDocumentType.ADDITIONAL_DOCUMENT, multipartFile);
  }

  @GetMapping("download/{uploadedFileId}")
  @ResponseBody
  public ResponseEntity<InputStreamResource> download(@PathVariable("scapId") ScapId scapId,
                                                      @PathVariable("uploadedFileId") UUID uploadedFileId) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);
    var uploadedFile = supportingDocumentService.getUploadedFile(scapDetail, uploadedFileId);
    var inputStream = fileUploadService.downloadFile(uploadedFile);
    return FileUploadUtils.getFileResourceResponseEntity(uploadedFile, new InputStreamResource(inputStream));
  }

  @PostMapping("delete/{uploadedFileId}")
  @ResponseBody
  public FileDeleteResult delete(@PathVariable("scapId") ScapId scapId,
                                 @PathVariable("uploadedFileId") UUID uploadedFileId) {
    var scapDetail = scapDetailService.getLatestByScapId(scapId);
    return supportingDocumentService.deleteFile(scapDetail, uploadedFileId);
  }
}
