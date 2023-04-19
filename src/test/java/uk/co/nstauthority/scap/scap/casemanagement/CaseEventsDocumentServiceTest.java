package uk.co.nstauthority.scap.scap.casemanagement;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.io.IOException;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.multipart.MultipartFile;
import uk.co.nstauthority.scap.file.FileTestUtil;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.FileUploadValidationService;
import uk.co.nstauthority.scap.file.UploadErrorType;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.file.VirtualFolder;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocument;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
class CaseEventsDocumentServiceTest {

  @Mock
  private FileUploadService fileUploadService;
  @Mock
  private FileUploadValidationService fileUploadValidationService;

  @InjectMocks
  private CaseEventDocumentService caseEventDocumentService;
  private static final String filename = FileTestUtil.VALID_FILENAME;
  private static final long fileSize = FileTestUtil.VALID_FILE_SIZE;
  private static final String contentType = FileTestUtil.VALID_CONTENT_TYPE;
  private static final VirtualFolder virtualFolder = FileTestUtil.VALID_VIRTUAL_FOLDER;
  private static final UploadedFile uploadedFile = FileTestUtil.createValidUploadedFile();
  private static MultipartFile multipartFile;
  private static ScapId scapId;
  private static ScapDetail scapDetail;

  @BeforeAll
  static void beforeAll() throws IOException {
    multipartFile = FileTestUtil.createMultipartFileMock();
    scapDetail = new ScapDetail(35);
    scapId = new ScapId(12);
  }

  @ParameterizedTest
  @EnumSource(UploadErrorType.class)
  void processFileUpload_FailsValidation_ReturnsInvalidResult(UploadErrorType uploadErrorType) {
    var submissionDetail = mock(ScapDetail.class);
    when(fileUploadService.sanitiseFilename(filename)).thenReturn(filename);
    when(fileUploadValidationService.validateFileUpload(multipartFile, fileSize, filename))
        .thenReturn(uploadErrorType);

    var fileUploadResult = caseEventDocumentService.processFileUpload(multipartFile);

    assertThat(fileUploadResult.isValid()).isFalse();
  }

  @Test
  void processFileUpload_PassesValidationAndUploadsFile_VerifyInteractions() {
    var submissionDetail = mock(ScapDetail.class);
    when(fileUploadService.sanitiseFilename(filename)).thenReturn(filename);
    doReturn(uploadedFile)
        .when(fileUploadService)
        .createUploadedFile(virtualFolder, fileSize, filename, contentType);

    var fileUploadResult = caseEventDocumentService.processFileUpload(multipartFile);

    assertThat(fileUploadResult.isValid()).isTrue();
  }

  @Test
  void deleteFile_FileDeleted_SupportingDocAndFileDeleted() {
    var uuid = UUID.randomUUID();
    when(fileUploadService.getUploadedFile(uuid)).thenReturn(uploadedFile);

    var fileDeleteResult = caseEventDocumentService.deleteFile(uuid);

    verify(fileUploadService, times(1)).deleteFile(uploadedFile);
    assertThat(fileDeleteResult.isValid()).isTrue();
  }

  @Test
  void buildFileUploadTemplate_AdditionDocumentType() {
    var supportingDocumentType = SupportingDocumentType.ADDITIONAL_DOCUMENT;

    var fileUploadTemplate = caseEventDocumentService.buildFileUploadTemplate(
        scapId, supportingDocumentType);

    verify(fileUploadService).buildFileUploadTemplate(
        ReverseRouter.route(on(CaseEventsDocumentController.class).download(scapId, null)),
        ReverseRouter.route(on(CaseEventsDocumentController.class)
            .upload(scapId, supportingDocumentType, null)),
        ReverseRouter.route(on(CaseEventsDocumentController.class).delete(scapId, null))
    );
  }

  @Test
  void buildFileUploadTemplate_ConsultationResponseType() {
    var supportingDocumentType = SupportingDocumentType.CONSULTATION_REPORT;

    var fileUploadTemplate = caseEventDocumentService.buildFileUploadTemplate(
        scapId, supportingDocumentType);

    verify(fileUploadService).buildFileUploadTemplate(
        ReverseRouter.route(on(CaseEventsDocumentController.class).download(scapId,null)),
        ReverseRouter.route(on(CaseEventsDocumentController.class)
            .upload(scapId,supportingDocumentType, null)),
        ReverseRouter.route(on(CaseEventsDocumentController.class).delete(scapId,null))
    );
  }

  @Test
  void updateSupportingDocumentDescriptions() {
    var fileUploadForm = mock(FileUploadForm.class);
    var fileUploadFormList = List.of(fileUploadForm);

    caseEventDocumentService.updateSupportingDocumentFileDescriptions(fileUploadFormList);

    verify(fileUploadService).updateFileUploadDescriptions(fileUploadFormList);
  }

  @Test
  void findUploadedFileOrThrow() {
    var supportingDocument = createSupportingDocument(
        UUID.randomUUID(), SupportingDocumentType.ADDITIONAL_DOCUMENT);
    supportingDocument.setUploadedFile(uploadedFile);

    when(fileUploadService.getUploadedFile(uploadedFile.getId())).thenReturn(uploadedFile);
    var returnedUploadedFile = caseEventDocumentService.getUploadedFile(uploadedFile.getId());

    assertThat(returnedUploadedFile).isEqualTo(uploadedFile);
  }

  private SupportingDocument createSupportingDocument(UUID uuid, SupportingDocumentType supportingDocumentType) {
    var supportingDocument = new SupportingDocument();
    supportingDocument.setId(uuid);
    supportingDocument.setSupportingDocumentType(supportingDocumentType);
    supportingDocument.setUploadedFile(uploadedFile);
    return supportingDocument;
  }

  private FileUploadForm createFileUploadForm() {
    var fileUploadForm = new FileUploadForm();
    fileUploadForm.setUploadedFileId(CaseEventsDocumentServiceTest.uploadedFile.getId());
    fileUploadForm.setUploadedFileDescription(CaseEventsDocumentServiceTest.uploadedFile.getDescription());
    fileUploadForm.setUploadedFileInstant(CaseEventsDocumentServiceTest.uploadedFile.getUploadedTimeStamp());
    return fileUploadForm;
  }
}
