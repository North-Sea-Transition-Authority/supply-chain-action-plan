package uk.co.nstauthority.scap.file;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Instant;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.multipart.MultipartFile;

@ExtendWith(MockitoExtension.class)
class FileUploadServiceTest {
  @Mock
  private FileUploadStorageService fileUploadStorageService;
  @Mock
  private UploadedFilePersistenceService uploadedFilePersistenceService;
  private final FileUploadConfig fileUploadConfig = FileTestUtil.validFileUploadConfig();
  private FileUploadService fileUploadService;
  private static final UploadedFile uploadedFile = FileTestUtil.createValidUploadedFile();
  private final Instant now = Instant.now();

  @BeforeEach
  void setUp() {
    fileUploadService =
        new FileUploadService(fileUploadStorageService, uploadedFilePersistenceService, fileUploadConfig);
  }

  @Test
  void uploadFile() {
    var multipartFile = Mockito.mock(MultipartFile.class);
    fileUploadService.uploadFile(multipartFile, uploadedFile);
    verify(fileUploadStorageService).uploadFile(multipartFile, uploadedFile);
  }

  @Test
  void createUploadedFile() {
    fileUploadService.createUploadedFile(FileTestUtil.VALID_VIRTUAL_FOLDER, FileTestUtil.VALID_FILE_SIZE, FileTestUtil.VALID_FILENAME,
        FileTestUtil.VALID_CONTENT_TYPE);
    verify(uploadedFilePersistenceService).createUploadedFile(FileTestUtil.VALID_VIRTUAL_FOLDER, FileTestUtil.VALID_FILE_SIZE, FileTestUtil.VALID_FILENAME,
        FileTestUtil.VALID_CONTENT_TYPE);
  }

  @Test
  void deleteFile_VerifyInteractions() {
    fileUploadService.deleteFile(uploadedFile);

    verify(fileUploadStorageService).deleteFile(uploadedFile);
    verify(uploadedFilePersistenceService).deleteFile(uploadedFile);
  }

  @SuppressWarnings("resource")
  @Test
  void downloadFile_VerifyInteractions() {
    fileUploadService.downloadFile(uploadedFile);
    verify(fileUploadStorageService).downloadFile(uploadedFile);
  }

  @Test
  void updateFileUploadDescriptions() {
    var uuid = UUID.randomUUID();
    var fileUploadForm = createFileUploadForm(uuid);
    when(fileUploadService.getUploadedFile(uuid)).thenReturn(uploadedFile);

    fileUploadService.updateFileUploadDescriptions(List.of(fileUploadForm));

    verify(uploadedFilePersistenceService, times(1)).updateFileDescription(uploadedFile, fileUploadForm.getUploadedFileDescription());
  }

  @Test
  void buildFileUploadTemplate() {
    var downloadUrl = "download";
    var uploadUrl = "upload";
    var deleteUrl = "delete";

    var fileUploadTemplate =
        fileUploadService.buildFileUploadTemplate(downloadUrl, uploadUrl, deleteUrl);

    assertThat(fileUploadTemplate)
        .extracting(
            FileUploadTemplate::downloadUrl,
            FileUploadTemplate::uploadUrl,
            FileUploadTemplate::deleteUrl,
            FileUploadTemplate::maxAllowedSize,
            FileUploadTemplate::allowedExtensions
        )
        .contains(
            downloadUrl,
            uploadUrl,
            deleteUrl,
            fileUploadConfig.maxAllowedSize().toString(),
            fileUploadConfig.allowedExtensions()
        );
  }

  @Test
  void sanitiseFilename() {
    var filename = "this%is%test.txt";

    var sanitiseFilename = fileUploadService.sanitiseFilename(filename);

    assertThat(sanitiseFilename).isEqualTo("this_is_test.txt");
  }

  @Test
  void getUploadedFileViewList() {
    var uuid = UUID.randomUUID();
    var fileUploadIdList = List.of(uuid);

    when(uploadedFilePersistenceService.getUploadedFilesByIdList(fileUploadIdList))
        .thenReturn(List.of(uploadedFile));

    var uploadedFileViews = fileUploadService.getUploadedFileViewList(fileUploadIdList);

    assertThat(uploadedFileViews)
        .extracting(
            UploadedFileView::getFileId,
            UploadedFileView::getFileName,
            UploadedFileView::getFileSize,
            UploadedFileView::getFileDescription,
            UploadedFileView::getFileUploadedTime
        )
        .contains(
            tuple(
                uploadedFile.getId().toString(),
                uploadedFile.getFilename(),
                FileUploadUtils.fileSizeFormatter(uploadedFile.getFileSizeBytes()),
                uploadedFile.getDescription(),
                uploadedFile.getUploadedTimeStamp()
            )
        );
  }

  @Test
  void createFileUploadForm() {
    var uploadedFile = new UploadedFile();
    uploadedFile.setId(UUID.randomUUID());
    uploadedFile.setDescription("description");
    uploadedFile.setUploadedTimeStamp(Instant.now());

    var form = fileUploadService.createFileUploadForm(uploadedFile);

    assertThat(form.getUploadedFileId()).isEqualTo(uploadedFile.getId());
    assertThat(form.getUploadedFileDescription()).isEqualTo(uploadedFile.getDescription());
  }

  private FileUploadForm createFileUploadForm(UUID uuid) {
    var fileUploadForm = new FileUploadForm();
    fileUploadForm.setUploadedFileId(uuid);
    fileUploadForm.setUploadedFileDescription("test description");
    fileUploadForm.setUploadedFileInstant(now);
    return fileUploadForm;
  }
}
