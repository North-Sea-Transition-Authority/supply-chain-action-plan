package uk.co.nstauthority.scap.scap.summary.files;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.AdditionalDocumentsController;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
class FileUploadSummaryViewServiceTest {

  @Mock
  SupportingDocumentService supportingDocumentService;

  @Mock
  FileUploadService fileUploadService;

  @InjectMocks
  FileUploadSummaryViewService fileUploadSummaryViewService;


  @Test
  void getAllByScapDetailAndDocumentType() {
    var scapId = new ScapId(1);
    var scap = mock(Scap.class);
    var scapDetail = mock(ScapDetail.class);
    var type = SupportingDocumentType.ADDITIONAL_DOCUMENT;
    var mockFileUpload = mock(FileUploadForm.class);
    var fileId = UUID.randomUUID();
    var fileName = "test.txt";
    var fileDesc = "test desc";
    var uploadedFile = new UploadedFile();
    uploadedFile.setFilename(fileName);
    uploadedFile.setDescription(fileDesc);
    uploadedFile.setId(fileId);

    doReturn(scap).when(scapDetail).getScap();
    doReturn(scapId).when(scap).getScapId();
    doReturn(Collections.singletonList(mockFileUpload)).when(supportingDocumentService)
        .getFileUploadFormListForScapDetailAndType(scapDetail, type);
    doReturn(fileId).when(mockFileUpload).getUploadedFileId();
    doReturn(uploadedFile).when(fileUploadService).findUploadedFileOrThrow(fileId);

    var fileUploadSummaryViews = fileUploadSummaryViewService
        .getAllByScapDetailAndDocumentType(scapDetail, type);

    assertThat(fileUploadSummaryViews).extracting(
        FileUploadSummaryView::fileName,
        FileUploadSummaryView::fileDescription,
        FileUploadSummaryView::fileUrl
    ).containsExactly(
        tuple(
            fileName,
            fileDesc,
            ReverseRouter.route(on(AdditionalDocumentsController.class).download(scapId, fileId))
        )
    );
  }
}
