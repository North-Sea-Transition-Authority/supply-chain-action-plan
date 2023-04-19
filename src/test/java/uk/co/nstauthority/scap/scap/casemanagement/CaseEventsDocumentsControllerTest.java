package uk.co.nstauthority.scap.scap.casemanagement;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.core.io.ClassPathResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
import uk.co.nstauthority.scap.file.FileTestUtil;
import uk.co.nstauthority.scap.file.FileUploadService;
import uk.co.nstauthority.scap.file.UploadedFile;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;

@ContextConfiguration(classes = CaseEventsDocumentController.class)
class CaseEventsDocumentsControllerTest extends AbstractScapSubmitterControllerTest {

  private static final ServiceUserDetail USER = ServiceUserDetailTestUtil.Builder().build();

  @MockBean
  CaseEventDocumentService caseEventDocumentService;

  @MockBean
  FileUploadService fileUploadService;

  private ScapDetail scapDetail;
  private SupportingDocumentType supportingDocumentType;
  private UploadedFile uploadedFile;

  @BeforeEach
  void setUp() {
    scapDetail = new ScapDetail(35);
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);
    when(scapDetailService.getLatestByScap(scap)).thenReturn(scapDetail);

    uploadedFile = FileTestUtil.createValidUploadedFile();

    supportingDocumentType = SupportingDocumentType.CONSULTATION_REPORT;
  }

  @Test
  void upload_assertStatusOk() throws Exception {
    when(scapDetailService.getLatestByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);

    MockMultipartFile mockMultipartFile = new MockMultipartFile("file", (byte[]) null);
    mockMvc.perform(multipart(ReverseRouter.route(
            on(CaseEventsDocumentController.class).upload(SCAP_ID,supportingDocumentType, null)))
            .file(mockMultipartFile)
            .with(user(USER))
            .with(csrf()))
        .andExpect(status().isOk());

    verify(caseEventDocumentService).processFileUpload(mockMultipartFile);
  }

  @Test
  void download_assertStatusOk() throws Exception {
    when(scapDetailService.getLatestByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);
    when(caseEventDocumentService.getUploadedFile(uploadedFile.getId())).thenReturn(uploadedFile);
    var fileResource = new ClassPathResource("banner.txt");
    when(fileUploadService.downloadFile(uploadedFile)).thenReturn(fileResource.getInputStream());

    var response = mockMvc.perform(
            get(ReverseRouter.route(
                on(CaseEventsDocumentController.class).download(SCAP_ID, uploadedFile.getId())))
                .with(user(USER)))
        .andExpect(status().isOk())
        .andReturn().getResponse();

    assertThat(response)
        .extracting(
            MockHttpServletResponse::getContentType,
            MockHttpServletResponse::getContentLengthLong,
            mockHttpServletResponse -> mockHttpServletResponse.getHeader(HttpHeaders.CONTENT_DISPOSITION)
        )
        .contains(
            MediaType.APPLICATION_OCTET_STREAM.toString(),
            uploadedFile.getFileSizeBytes(),
            "attachment; filename=\"%s\"".formatted(uploadedFile.getFilename())
        );
  }

  @Test
  void delete_assertStatusOk() throws Exception {
    when(scapDetailService.getLatestByScapIdOrThrow(SCAP_ID)).thenReturn(scapDetail);

    mockMvc.perform(post(
            ReverseRouter.route(on(CaseEventsDocumentController.class).delete(SCAP_ID, uploadedFile.getId())))
            .with(user(USER))
            .with(csrf()))
        .andExpect(status().isOk());

    verify(caseEventDocumentService, times(1)).deleteFile(uploadedFile.getId());
  }
}
