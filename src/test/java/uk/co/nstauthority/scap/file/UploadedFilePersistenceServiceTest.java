package uk.co.nstauthority.scap.file;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.file.s3.S3ClientService;

@ExtendWith(MockitoExtension.class)
class UploadedFilePersistenceServiceTest {

  @Mock
  private S3ClientService s3ClientService;
  @Mock
  private UploadedFileRepository uploadedFileRepository;
  @Mock
  private Clock clock;

  @InjectMocks
  private UploadedFilePersistenceService uploadedFilePersistenceService;

  @Test
  void createUploadedFile() {
    uploadedFilePersistenceService.createUploadedFile(FileTestUtil.VALID_VIRTUAL_FOLDER, FileTestUtil.VALID_FILE_SIZE, FileTestUtil.VALID_FILENAME,
        FileTestUtil.VALID_CONTENT_TYPE);
    verify(uploadedFileRepository).save(any(UploadedFile.class));
  }

  @Test
  void createUploadedFile_UploadedFileRecordSaved() {
    when(s3ClientService.getDefaultBucketName()).thenReturn(FileTestUtil.VALID_BUCKET_NAME);

    var argumentCaptor = ArgumentCaptor.forClass(UploadedFile.class);
    uploadedFilePersistenceService.createUploadedFile(FileTestUtil.VALID_VIRTUAL_FOLDER, FileTestUtil.VALID_FILE_SIZE, FileTestUtil.VALID_FILENAME,
        FileTestUtil.VALID_CONTENT_TYPE);

    verify(uploadedFileRepository).save(argumentCaptor.capture());
    var file = argumentCaptor.getValue();

    assertThat(file)
        .extracting(
            UploadedFile::getBucketName,
            UploadedFile::getVirtualFolder,
            UploadedFile::getFilename,
            UploadedFile::getFileContentType,
            UploadedFile::getFileSizeBytes,
            UploadedFile::getUploadedTimeStamp,
            UploadedFile::getDescription
        )
        .contains(
            FileTestUtil.VALID_BUCKET_NAME,
            FileTestUtil.VALID_VIRTUAL_FOLDER,
            FileTestUtil.VALID_FILENAME,
            FileTestUtil.VALID_CONTENT_TYPE,
            FileTestUtil.VALID_FILE_SIZE,
            clock.instant(),
            null
        );
  }

  @Test
  void findUploadedFileOrThrow_UploadedFileFound_VerifyInteractions() {
    var uuid = UUID.randomUUID();
    when(uploadedFileRepository.findById(uuid)).thenReturn(Optional.of(mock(UploadedFile.class)));
    var uploadedFile = uploadedFilePersistenceService.getUploadedFile(uuid);
    verify(uploadedFileRepository).findById(uuid);
  }

  @Test
  void findUploadedFileOrThrow_UploadedFileNotFound_VerifyThrowsException() {
    var uuid = UUID.randomUUID();
    when(uploadedFileRepository.findById(uuid)).thenReturn(Optional.empty());

    assertThatExceptionOfType(ScapEntityNotFoundException.class).isThrownBy(() -> {
      var uploadedFile = uploadedFilePersistenceService.getUploadedFile(uuid);
    });
  }

  @Test
  void getUploadedFilesByIdList_VerifyCalls() {
    var uuid = UUID.randomUUID();
    var uuidList = List.of(uuid);

    uploadedFilePersistenceService.getUploadedFilesByIdList(uuidList);

    verify(uploadedFileRepository, times(1)).findAllByIdIn(uuidList);
  }

  @Test
  void updateFileDescription() {
    var file = new UploadedFile();
    var uploadedFileDescription = "desc";

    assertThat(file.getDescription()).isNull();

    uploadedFilePersistenceService.updateFileDescription(file, uploadedFileDescription);

    verify(uploadedFileRepository, times(1)).save(file);
    assertThat(file.getDescription()).isEqualTo(uploadedFileDescription);
  }

  @Test
  void deleteFile() {
    var uploadedFile = mock(UploadedFile.class);
    uploadedFilePersistenceService.deleteFile(uploadedFile);
    verify(uploadedFileRepository).delete(uploadedFile);
  }
}
