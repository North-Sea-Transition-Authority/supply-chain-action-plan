package uk.co.nstauthority.scap.scap.casemanagement.approval;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.flash;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.file.FileUploadForm;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.notify.ScapEmailService;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventDocumentService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapApprovalController.class)
class ScapApprovalControllerTest extends AbstractControllerTest {

  @MockBean
  private CaseEventService caseEventService;

  @MockBean
  private ScapSummaryViewService scapSummaryViewService;

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private ScapApprovalFormValidator scapApprovalFormValidator;

  @MockBean
  CaseEventDocumentService caseEventDocumentService;

  @MockBean
  private ScapEmailService scapEmailService;

  private static final ScapId SCAP_ID = new ScapId(1111);

  private static final Integer ORG_GROUP_ID = 1000;

  private static final String TEST_STRING = "This is a test comment";

  private ScapDetail scapDetail = getScapDetail();

  @BeforeEach
  void setup() {
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.APPROVAL_DOCUMENT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.FURTHER_INFORMATION)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.values()));
    when(scapService.getScapById(SCAP_ID)).thenReturn(new Scap());
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(scapDetail);
    when(scapDetailService.getLatestByScap(any(Scap.class))).thenReturn(scapDetail);
    when(organisationGroupService.getOrganisationGroupById(eq(ORG_GROUP_ID), any())).thenReturn(Optional.of(getOrgGroup()));
    when(scapSummaryViewService.getScapSummaryView(any())).thenReturn(getScapSummaryView());
  }

  @Test
  void saveQaComments_ValidationSuccesful_saved() throws Exception {
    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.QA,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapApprovalForm", getScapApprovalForm()))
        .andExpect(status().is3xxRedirection());

    verify(caseEventService).recordNewEvent(CaseEventSubject.SCAP_APPROVED,
        scapDetail,
        1,
        getScapApprovalForm().getApprovalComments().getInputValue(),
        null);
    verify(scapDetailService).approveScap(scapDetail);
  }

  @Test
  void saveApproval_ValidationSuccesful_savedWithComments() throws Exception {
    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.APPROVED,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapApprovalForm", getScapApprovalForm()))
        .andExpect(status().is3xxRedirection())
        .andExpect(flash().attributeExists("notificationBannerView"));

    verify(caseEventService).recordNewEvent(CaseEventSubject.SCAP_APPROVED, scapDetail, 1, TEST_STRING, null);
    verify(scapDetailService).approveScap(scapDetail);
    verify(scapEmailService).sendScapApprovalEmails(scapDetail, null, false);
  }

  @Test
  void saveApproval_ValidationSuccesful_savedWithFile() throws Exception {
    var form = getScapApprovalForm();
    var file = new FileUploadForm();
    file.setUploadedFileId(UUID.randomUUID());
    file.setUploadedFileDescription("Description");
    form.setApprovalDocuments(List.of(file));
    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.APPROVED,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapApprovalForm", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(flash().attributeExists("notificationBannerView"));

    verify(caseEventService).recordNewEvent(CaseEventSubject.SCAP_APPROVED, scapDetail, 1, TEST_STRING, file.getUploadedFileId());
    verify(scapDetailService).approveScap(scapDetail);
    verify(scapEmailService).sendScapApprovalEmails(scapDetail, null, false);
  }

  @Test
  void saveApproval_ValidationFailed_Reroute() throws Exception {
    doAnswer(invocation -> {
      var bindingResult = (BindingResult) invocation.getArgument(1);
      bindingResult.rejectValue("approvalComments.inputValue", "testError", "This is an error message");
      return bindingResult;
    }).when(scapApprovalFormValidator).validate(any(), any());

    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.INFO_REQUESTED,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf()))
        .andExpect(status().isOk());

    verify(caseEventService, never()).recordNewEvent(CaseEventSubject.SCAP_APPROVED, scapDetail, 1, TEST_STRING);
    verifyNoInteractions(scapEmailService);
  }

  private ScapApprovalForm getScapApprovalForm() {
    var form = new ScapApprovalForm();
    var input = form.getApprovalComments();
    input.setInputValue(TEST_STRING);

    form.setApprovalComments(input);
    form.setProjectClosedOut(YesNo.NO);
    return form;
  }

  private ScapDetail getScapDetail() {
    var scapDetail = new ScapDetail();
    scapDetail.setVersionNumber(1);
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);

    var scap = new Scap(SCAP_ID);
    scap.setOrganisationGroupId(ORG_GROUP_ID);
    scapDetail.setScap(scap);

    return scapDetail;
  }

  private OrganisationGroup getOrgGroup() {
    var orgGroup = new OrganisationGroup();
    return orgGroup;
  }
}
