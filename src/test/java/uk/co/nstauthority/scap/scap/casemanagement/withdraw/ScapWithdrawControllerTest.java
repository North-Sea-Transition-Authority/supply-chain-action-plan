package uk.co.nstauthority.scap.scap.casemanagement.withdraw;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.flash;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.SUBMITTED;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BindingResult;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.file.FileUploadTemplate;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.notify.ScapEmailService;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventDocumentService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.supportingdocuments.SupportingDocumentType;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapWithdrawController.class)
class ScapWithdrawControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  private CaseEventService caseEventService;

  @SpyBean
  private ControllerHelperService controllerHelperService;

  @MockBean
  private ScapSummaryViewService scapSummaryViewService;

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private ScapWithdrawalFormValidator scapWithdrawalFormValidator;

  @MockBean
  private CaseEventDocumentService caseEventDocumentService;

  @MockBean
  private ScapEmailService scapEmailService;

  private static final ScapDetail SCAP_DETAIL = getScapDetail();
  private static final Integer ORG_GROUP_ID = 1000;
  private static final String TEST_STRING = "This is a test comment";

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
    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(SCAP_DETAIL);
    when(scapDetailService.getLatestByScap(scap)).thenReturn(SCAP_DETAIL);
    when(scapDetailService.getLatestSubmittedScapDetail(SCAP_ID)).thenReturn(SCAP_DETAIL);
    when(organisationGroupService.getOrganisationGroupById(eq(ORG_GROUP_ID), any()))
        .thenReturn(Optional.of(OrganisationGroup.newBuilder().build()));
    when(scapSummaryViewService.getScapSummaryView(any())).thenReturn(getScapSummaryView());
  }

  @Test
  void withdrawScap_ValidationSuccesful_savedWithComments() throws Exception {
    mockMvc.perform(post(ReverseRouter.route(on(ScapWithdrawController.class)
            .withdrawScap(
                SCAP_ID,
                CaseEventAction.INFO_RESPONSE,
                false,
                getWithdrawalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapWithdrawForm", getWithdrawalForm()))
        .andExpect(status().is3xxRedirection())
        .andExpect(flash().attributeExists("notificationBannerView"));

    verify(caseEventService).recordNewEvent(CaseEventSubject.SCAP_WITHDRAWN, SCAP_DETAIL, 1, TEST_STRING);
    verify(scapEmailService).sendScapWithdrawalEmails(SCAP_DETAIL);
  }

  @Test
  void withdrawScap_ValidationFailed_Reroute() throws Exception {
    doAnswer(invocation -> {
      var bindingResult = (BindingResult) invocation.getArgument(1);
      bindingResult.rejectValue("withdrawComments.inputValue", "testError", "This is an error message");
      return bindingResult;
    }).when(scapWithdrawalFormValidator).validate(any(), any());

    mockMvc.perform(post(ReverseRouter.route(on(ScapWithdrawController.class)
            .withdrawScap(
                SCAP_ID,
                CaseEventAction.INFO_RESPONSE,
                false,
                getWithdrawalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf()))
        .andExpect(status().isOk());

    verify(caseEventService, never()).recordNewEvent(CaseEventSubject.FURTHER_INFO_RESPONSE, SCAP_DETAIL, 1, TEST_STRING);
  }

  private ScapWithdrawalForm getWithdrawalForm() {
    var form = new ScapWithdrawalForm();
    var input = form.getWithdrawComments();
    input.setInputValue(TEST_STRING);

    form.setWithdrawComments(input);
    return form;
  }

  private static ScapDetail getScapDetail() {
    var scapDetail = new ScapDetail();
    scapDetail.setVersionNumber(1);
    scapDetail.setStatus(SUBMITTED);

    var scap = new Scap(SCAP_ID);
    scap.setOrganisationGroupId(ORG_GROUP_ID);
    scapDetail.setScap(scap);

    return scapDetail;
  }
}
