package uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.io.IOException;
import java.util.Collections;
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
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
class SupportingDocumentServiceTest {

  @Mock
  private FileUploadService fileUploadService;
  @Mock
  private FileUploadValidationService fileUploadValidationService;
  @Mock
  private SupportingDocumentRepository supportingDocumentRepository;

  @InjectMocks
  private SupportingDocumentService supportingDocumentService;
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

    var fileUploadResult = supportingDocumentService.processFileUpload(submissionDetail, SupportingDocumentType.ADDITIONAL_DOCUMENT, multipartFile);

    assertThat(fileUploadResult.isValid()).isFalse();
  }

  @Test
  void processFileUpload_PassesValidationAndUploadsFile_VerifyInteractions() {
    var submissionDetail = mock(ScapDetail.class);
    when(fileUploadService.sanitiseFilename(filename)).thenReturn(filename);
    doReturn(uploadedFile)
        .when(fileUploadService)
        .createUploadedFile(virtualFolder, fileSize, filename, contentType);

    var fileUploadResult = supportingDocumentService.processFileUpload(submissionDetail, SupportingDocumentType.ADDITIONAL_DOCUMENT, multipartFile);

    assertThat(fileUploadResult.isValid()).isTrue();
  }

  @Test
  void deleteFile_FileDeleted_SupportingDocAndFileDeleted() {
    var uuid = UUID.randomUUID();
    var supportingDocument = mock(SupportingDocument.class);
    when(fileUploadService.findUploadedFileOrThrow(uuid)).thenReturn(uploadedFile);
    when(supportingDocumentRepository.findByScapDetailAndUploadedFile(scapDetail, uploadedFile))
        .thenReturn(supportingDocument);
    when(supportingDocumentRepository.findAllByUploadedFile(uploadedFile))
        .thenReturn(Collections.singletonList(supportingDocument));

    var fileDeleteResult = supportingDocumentService.deleteFile(scapDetail, uuid);

    verify(supportingDocumentRepository, times(1)).delete(supportingDocument);
    verify(fileUploadService, times(1)).deleteFile(uploadedFile);
    assertThat(fileDeleteResult.isValid()).isTrue();
  }

  @Test
  void deleteFile_FileDeletedButExistsInOtherVersion_OnlySupportingDocDeleted() {
    var supportingDocument1 = mock(SupportingDocument.class);
    var supportingDocument2 = mock(SupportingDocument.class);
    when(fileUploadService.findUploadedFileOrThrow(uploadedFile.getId())).thenReturn(uploadedFile);
    when(supportingDocumentRepository.findByScapDetailAndUploadedFile(scapDetail, uploadedFile))
        .thenReturn(supportingDocument1);
    when(supportingDocumentRepository.findAllByUploadedFile(uploadedFile))
        .thenReturn(List.of(supportingDocument1, supportingDocument2));

    var fileDeleteResult = supportingDocumentService.deleteFile(scapDetail, uploadedFile.getId());

    verify(supportingDocumentRepository, times(1)).delete(supportingDocument1);
    verify(fileUploadService, never()).deleteFile(uploadedFile);
    assertThat(fileDeleteResult.isValid()).isTrue();
  }

  @Test
  void getFileUploadFormListForSubmissionAndType() {
    var uuid = UUID.randomUUID();
    var supportingDocumentType = SupportingDocumentType.ADDITIONAL_DOCUMENT;
    var supportingDocument = createSupportingDocument(uuid, supportingDocumentType);

    when(supportingDocumentRepository.findAllByScapDetailAndSupportingDocumentType(scapDetail,
        supportingDocumentType)).thenReturn(List.of(supportingDocument));
    when(fileUploadService.createFileUploadForm(uploadedFile)).thenReturn(createFileUploadForm());

    var fileUploadForms = supportingDocumentService.getFileUploadFormListForScapDetailAndType(
        scapDetail, supportingDocumentType);

    assertThat(fileUploadForms)
        .extracting(
            FileUploadForm::getUploadedFileId,
            FileUploadForm::getUploadedFileDescription,
            FileUploadForm::getUploadedFileInstant
        )
        .contains(
            tuple(
                uploadedFile.getId(),
                uploadedFile.getDescription(),
                uploadedFile.getUploadedTimeStamp()
            )
        );
  }

  @Test
  void getUploadedFileViewList() {
    var uuid = UUID.randomUUID();
    var fileUploadIdList = List.of(uuid);

    var uploadedFileViews = supportingDocumentService.getUploadedFileViewList(fileUploadIdList);

    verify(fileUploadService).getUploadedFileViewList(fileUploadIdList);
  }

  @Test
  void buildFileUploadTemplate_AdditionDocumentType() {
    var supportingDocumentType = SupportingDocumentType.ADDITIONAL_DOCUMENT;

    var fileUploadTemplate = supportingDocumentService.buildFileUploadTemplate(
        scapId, supportingDocumentType);

    verify(fileUploadService).buildFileUploadTemplate(
        ReverseRouter.route(on(AdditionalDocumentsController.class).download(scapId, null)),
        ReverseRouter.route(on(AdditionalDocumentsController.class)
            .upload(scapId, null)),
        ReverseRouter.route(on(AdditionalDocumentsController.class).delete(scapId, null))
    );
  }

  @Test
  void buildFileUploadTemplate_ConsultationResponseType() {
    var supportingDocumentType = SupportingDocumentType.CONSULTATION_REPORT;

    var fileUploadTemplate = supportingDocumentService.buildFileUploadTemplate(
        scapId, supportingDocumentType);

    verify(fileUploadService).buildFileUploadTemplate(
        ReverseRouter.route(on(ConsultationDocumentsController.class).download(scapId, null)),
        ReverseRouter.route(on(ConsultationDocumentsController.class)
            .upload(scapId, null)),
        ReverseRouter.route(on(ConsultationDocumentsController.class).delete(scapId, null))
    );
  }

  @Test
  void updateSupportingDocumentDescriptions() {
    var fileUploadForm = mock(FileUploadForm.class);
    var fileUploadFormList = List.of(fileUploadForm);

    supportingDocumentService.updateSupportingDocumentFileDescriptions(fileUploadFormList);

    verify(fileUploadService).updateFileUploadDescriptions(fileUploadFormList);
  }

  @Test
  void findUploadedFileOrThrow() {
    var supportingDocument = createSupportingDocument(
        UUID.randomUUID(), SupportingDocumentType.ADDITIONAL_DOCUMENT);
    supportingDocument.setUploadedFile(uploadedFile);

    when(fileUploadService.findUploadedFileOrThrow(uploadedFile.getId())).thenReturn(uploadedFile);
    when(supportingDocumentRepository.findByScapDetailAndUploadedFile(scapDetail, uploadedFile))
        .thenReturn(supportingDocument);

    var returnedUploadedFile = supportingDocumentService.findUploadedFileOrThrow(scapDetail, uploadedFile.getId());

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
    fileUploadForm.setUploadedFileId(SupportingDocumentServiceTest.uploadedFile.getId());
    fileUploadForm.setUploadedFileDescription(SupportingDocumentServiceTest.uploadedFile.getDescription());
    fileUploadForm.setUploadedFileInstant(SupportingDocumentServiceTest.uploadedFile.getUploadedTimeStamp());
    return fileUploadForm;
  }
}
