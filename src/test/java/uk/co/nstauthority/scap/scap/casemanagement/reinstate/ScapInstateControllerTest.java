package uk.co.nstauthority.scap.scap.casemanagement.reinstate;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
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
import static uk.co.nstauthority.scap.scap.detail.ScapDetailStatus.WITHDRAWN;
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
import uk.co.nstauthority.scap.AbstractControllerTest;
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
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapReinstateController.class)
class ScapInstateControllerTest extends AbstractControllerTest {

  @MockBean
  private CaseEventService caseEventService;

  @SpyBean
  private ControllerHelperService controllerHelperService;

  @MockBean
  private ScapSummaryViewService scapSummaryViewService;

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private ScapReinstateFormValidator scapReinstateFormValidator;

  @MockBean
  private CaseEventDocumentService caseEventDocumentService;

  @MockBean
  private ScapEmailService scapEmailService;

  private static final ScapId SCAP_ID = new ScapId(1111);

  private static final ScapDetail SCAP_DETAIL = getScapDetail();
  private static final Integer ORG_GROUP_ID = 1000;
  private static final String TEST_STRING = "This is a test comment";

  @BeforeEach
  void setup() {
    var scap = new Scap();

    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.CONSULTATION_REPORT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.APPROVAL_DOCUMENT)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(caseEventDocumentService.buildFileUploadTemplate(any(), eq(SupportingDocumentType.FURTHER_INFORMATION)))
        .thenReturn(new FileUploadTemplate("blank", "blank", "blank", "250", "txt"));
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.values()));
    when(scapService.getScapById(anyInt())).thenReturn(scap);
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(SCAP_DETAIL);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(SCAP_DETAIL);
    when(scapDetailService.getLatestSubmittedScapDetail(SCAP_ID)).thenReturn(SCAP_DETAIL);
    when(organisationGroupService.getOrganisationGroupById(eq(ORG_GROUP_ID), any()))
        .thenReturn(Optional.of(OrganisationGroup.newBuilder().build()));
    when(scapSummaryViewService.getScapSummaryView(any())).thenReturn(getScapSummaryView());
  }

  @Test
  void withdrawScap_ValidationSuccesful_savedWithComments() throws Exception {
    mockMvc.perform(post(ReverseRouter.route(on(ScapReinstateController.class)
            .reinstateScap(
                SCAP_ID,
                CaseEventAction.REINSTATE,
                false,
                getReinstateForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapReinstateForm", getReinstateForm()))
        .andExpect(status().is3xxRedirection())
        .andExpect(flash().attributeExists("notificationBannerView"));

    verify(caseEventService).recordNewEvent(CaseEventSubject.SCAP_REINSTATED, SCAP_DETAIL, 1, TEST_STRING);
  }

  @Test
  void withdrawScap_ValidationFailed_Reroute() throws Exception {
    doAnswer(invocation -> {
      var bindingResult = (BindingResult) invocation.getArgument(1);
      bindingResult.rejectValue("reinstateComments.inputValue", "testError", "This is an error message");
      return bindingResult;
    }).when(scapReinstateFormValidator).validate(any(), any());

    mockMvc.perform(post(ReverseRouter.route(on(ScapReinstateController.class)
            .reinstateScap(
                SCAP_ID,
                CaseEventAction.INFO_RESPONSE,
                false,
                getReinstateForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf()))
        .andExpect(status().isOk());

    verify(caseEventService, never()).recordNewEvent(CaseEventSubject.FURTHER_INFO_RESPONSE, SCAP_DETAIL, 1, TEST_STRING);
  }

  private ScapReinstateForm getReinstateForm() {
    var form = new ScapReinstateForm();
    var input = form.getReinstateComments();
    input.setInputValue(TEST_STRING);

    form.setReinstateComments(input);
    return form;
  }

  private static ScapDetail getScapDetail() {
    var scapDetail = new ScapDetail();
    scapDetail.setVersionNumber(1);
    scapDetail.setStatus(WITHDRAWN);

    var scap = new Scap(SCAP_ID);
    scap.setOrganisationGroupId(ORG_GROUP_ID);
    scapDetail.setScap(scap);

    return scapDetail;
  }
}
