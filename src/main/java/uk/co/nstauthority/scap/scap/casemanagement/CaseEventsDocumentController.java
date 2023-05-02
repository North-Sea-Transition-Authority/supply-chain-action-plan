package uk.co.nstauthority.scap.scap.casemanagement;

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
import uk.co.nstauthority.scap.endpointvalidation.annotations.UserHasAnyPermission;
import uk.co.nstauthority.scap.file.FileDeleteResult;
import uk.co.nstauthority.scap.file.FileUploadResult;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.FileUploadUtils;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@RestController
@RequestMapping("/{scapId}/case-events/")
@UserHasAnyPermission(permissions = RolePermission.REVIEW_SCAP)
public class CaseEventsDocumentController {

  private final CaseEventDocumentService caseEventDocumentService;
  private final ScapDetailService scapDetailService;
  private final FileUploadService fileUploadService;

  @Autowired
  public CaseEventsDocumentController(CaseEventDocumentService caseEventDocumentService,
                                      ScapDetailService scapDetailService,
                                      FileUploadService fileUploadService) {
    this.caseEventDocumentService = caseEventDocumentService;
    this.scapDetailService = scapDetailService;
    this.fileUploadService = fileUploadService;
  }

  @PostMapping("upload/{supportingDocumentType}")
  @ResponseBody
  public FileUploadResult upload(@PathVariable("scapId") ScapId scapId,
                                 @PathVariable("supportingDocumentType") SupportingDocumentType supportingDocumentType,
                                 @RequestParam("file") MultipartFile multipartFile) {
    return caseEventDocumentService.processFileUpload(multipartFile);
  }

  @GetMapping("download/{uploadedFileId}")
  @ResponseBody
  public ResponseEntity<InputStreamResource> download(@PathVariable("scapId") ScapId scapId,
                                                      @PathVariable("uploadedFileId") UUID uploadedFileId) {
    var uploadedFile = caseEventDocumentService.getUploadedFile(uploadedFileId);
    var inputStream = fileUploadService.downloadFile(uploadedFile);
    return FileUploadUtils.getFileResourceResponseEntity(uploadedFile, new InputStreamResource(inputStream));
  }

  @PostMapping("delete/{uploadedFileId}")
  @ResponseBody
  public FileDeleteResult delete(@PathVariable("scapId") ScapId scapId,
                                 @PathVariable("uploadedFileId") UUID uploadedFileId) {
    return caseEventDocumentService.deleteFile(uploadedFileId);
  }
}
