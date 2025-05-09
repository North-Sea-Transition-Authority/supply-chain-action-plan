package uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments;

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

import java.util.List;
import java.util.UUID;
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
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRole;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@ContextConfiguration(classes = AdditionalDocumentsController.class)
class AdditionalDocumentsControllerTest extends AbstractScapSubmitterControllerTest {

  private static final ServiceUserDetail USER = ServiceUserDetailTestUtil.Builder().build();

  @MockBean
  SupportingDocumentService supportingDocumentService;

  @MockBean
  FileUploadService fileUploadService;

  private ScapDetail scapDetail;
  private SupportingDocumentType supportingDocumentType;
  private UploadedFile uploadedFile;

  @BeforeEach
  void setUp() {
    scapDetail = new ScapDetail(35);
    uploadedFile = FileTestUtil.createValidUploadedFile();

    supportingDocumentType = SupportingDocumentType.ADDITIONAL_DOCUMENT;
  }

  @Test
  void download_whenCantAccessScap_assertForbidden() throws Exception {
    givenUserCantAccessScap();
    mockMvc.perform(
            get(ReverseRouter.route(
                on(AdditionalDocumentsController.class).download(SCAP_ID, uploadedFile.getId())))
                .with(user(USER)))
        .andExpect(status().isForbidden());
  }

  @Test
  void upload_assertStatusOk() throws Exception {
    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    MockMultipartFile mockMultipartFile = new MockMultipartFile("file", (byte[]) null);
    mockMvc.perform(multipart(ReverseRouter.route(
            on(AdditionalDocumentsController.class).upload(SCAP_ID, null)))
            .file(mockMultipartFile)
            .with(user(USER))
            .with(csrf()))
        .andExpect(status().isOk());

    verify(supportingDocumentService).processFileUpload(scapDetail, supportingDocumentType, mockMultipartFile);
  }

  @Test
  void download_assertStatusOk() throws Exception {
    givenUserCanAccessScap();
    when(supportingDocumentService.getUploadedFile(scapDetail, uploadedFile.getId())).thenReturn(uploadedFile);
    var fileResource = new ClassPathResource("banner.txt");
    when(fileUploadService.downloadFile(uploadedFile)).thenReturn(fileResource.getInputStream());

    var response = mockMvc.perform(
            get(ReverseRouter.route(
                on(AdditionalDocumentsController.class).download(SCAP_ID, uploadedFile.getId())))
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
    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);    mockMvc.perform(post(
            ReverseRouter.route(on(AdditionalDocumentsController.class).delete(SCAP_ID, uploadedFile.getId())))
            .with(user(USER))
            .with(csrf()))
        .andExpect(status().isOk());

    verify(supportingDocumentService, times(1)).deleteFile(scapDetail, uploadedFile.getId());
  }

  private void givenUserCanAccessScap() {
    scapDetail.setScap(getScap());
    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    var industryTeam = getTeam();
    var regulatorTeam = new Team();

    when(teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);
    when(teamService.getRegulatorTeam()).thenReturn(regulatorTeam);

    var teamMemberRole = new TeamMemberRole(UUID.randomUUID());
    teamMemberRole.setRole(IndustryTeamRole.SCAP_SUBMITTER.getEnumName());
    teamMemberRole.setTeam(industryTeam);
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), webUserAccountId))
        .thenReturn(List.of(teamMemberRole));
  }

  private void givenUserCantAccessScap() {
    scapDetail.setScap(getScap());
    when(scapDetailService.getLatestByScapId(SCAP_ID)).thenReturn(scapDetail);
    var industryTeam = getTeam();
    var regulatorTeam = new Team();
    regulatorTeam.setTeamType(TeamType.REGULATOR);

    when(teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);
    when(teamService.getRegulatorTeam()).thenReturn(regulatorTeam);

    var teamMemberRole = new TeamMemberRole(UUID.randomUUID());
    teamMemberRole.setRole(RegulatorTeamRole.ACCESS_MANAGER.getEnumName());
    teamMemberRole.setTeam(regulatorTeam);
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), webUserAccountId))
        .thenReturn(List.of(teamMemberRole));
  }
}
