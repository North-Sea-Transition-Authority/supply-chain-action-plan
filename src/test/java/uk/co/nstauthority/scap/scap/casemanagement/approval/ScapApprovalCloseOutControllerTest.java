package uk.co.nstauthority.scap.scap.casemanagement.approval;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.flash;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
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
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = ScapApprovalController.class)
class ScapApprovalCloseOutControllerTest extends AbstractControllerTest {

  @MockBean
  private CaseEventService caseEventService;

  @MockBean
  private ScapSummaryViewService scapSummaryViewService;

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private ScapApprovalFormValidator consultationRequestFormValidator;

  @MockBean
  private CaseEventDocumentService caseEventDocumentService;

  @MockBean
  private ScapEmailService scapEmailService;

  private static final ScapId SCAP_ID = new ScapId(1111);

  private static final Integer ORG_GROUP_ID = 1000;

  private static final String TEST_STRING = "This is a test comment";

  private static final String PURPOSE = "Get Org Group for Summary of SCAP ID: %s".formatted(SCAP_ID.scapId());

  private ScapDetail scapDetail;

  private Scap scap = new Scap(SCAP_ID);

  @BeforeEach
  void setup() {
    scapDetail = getScapDetail();
    when(caseEventDocumentService.buildFileUploadTemplate(SCAP_ID, SupportingDocumentType.APPROVAL_DOCUMENT))
        .thenReturn(new FileUploadTemplate("TEST", "TEST", "TEST", "100", ".xml"));
    when(caseEventDocumentService.buildFileUploadTemplate(SCAP_ID, SupportingDocumentType.CONSULTATION_REPORT))
        .thenReturn(new FileUploadTemplate("TEST", "TEST", "TEST", "100", ".xml"));
    when(caseEventDocumentService.buildFileUploadTemplate(SCAP_ID, SupportingDocumentType.FURTHER_INFORMATION))
        .thenReturn(new FileUploadTemplate("TEST", "TEST", "TEST", "100", ".xml"));
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.values()));
    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(scapService.getScapById(SCAP_ID.scapId())).thenReturn(scap);
    when(scapDetailService.getActionableScapDetail(SCAP_ID,testUser)).thenReturn(scapDetail);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(organisationGroupService.getOrganisationGroupById(ORG_GROUP_ID, PURPOSE)).thenReturn(Optional.of(getOrgGroup()));
    when(scapSummaryViewService.getScapSummaryView(scapDetail)).thenReturn(getScapSummaryView());
  }

  @Test
  void approveScap_CloseOut_ValidationSuccesful_savedWithComments() throws Exception {
    var expectedRedirect = ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID));

    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.CLOSED_OUT,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapApprovalForm", getScapApprovalForm())
            .flashAttr("scapId", SCAP_ID))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirect))
        .andExpect(flash().attributeExists("notificationBannerView"));
    verify(caseEventService).recordNewEvent(CaseEventSubject.SCAP_CLOSED_OUT, scapDetail, 1, TEST_STRING, null);
    verify(scapDetailService).closeOutScap(scapDetail);
    verify(scapEmailService).sendScapApprovalEmails(scapDetail, null, true);
  }

  @Test
  void saveQaComments_ValidationFailed_Reroute() throws Exception {
    doAnswer(invocation -> {
      var bindingResult = (BindingResult) invocation.getArgument(1);
      bindingResult.rejectValue("approvalComments.inputValue", "testError", "This is an error message");
      return bindingResult;
    }).when(consultationRequestFormValidator).validate(any(), any());

    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.CONSULTATION_REQUESTED,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapId", SCAP_ID))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/summary/scapSummaryOverview"));
    verify(caseEventService, never()).recordNewEvent(CaseEventSubject.SCAP_CLOSED_OUT, scapDetail, 1, TEST_STRING);
    verifyNoInteractions(scapEmailService);
  }

  @Test
  void approveScap_VersionSelect_Regulator_NoDrafts() throws Exception {
    var expectedRedirect = ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(true);
    when(scapDetailService.getActionableScapDetail(scapDetail.getScap().getScapId(), testUser)).thenReturn(scapDetail);

    var expectedResults = getScapDetails();
    expectedResults.remove(0);
    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.CLOSED_OUT,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapApprovalForm", getScapApprovalForm())
            .flashAttr("scapId", SCAP_ID))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirect));
  }

  @Test
  void approveScap_VersionSelect_Industry_DraftsIncluded() throws Exception {
    var expectedRedirect = ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_ID));
    when(teamService.userIsMemberOfRegulatorTeam(testUser)).thenReturn(false);
    when(scapDetailService.findAllByScap(scapDetail.getScap())).thenReturn(getScapDetails());

    mockMvc.perform(post(ReverseRouter.route(on(ScapApprovalController.class)
            .saveScapApprovalForm(
                SCAP_ID,
                CaseEventAction.CLOSED_OUT,
                false,
                getScapApprovalForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("scapApprovalForm", getScapApprovalForm())
            .flashAttr("scapId", SCAP_ID))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(expectedRedirect));
  }

  private ScapApprovalForm getScapApprovalForm() {
    var form = new ScapApprovalForm();
    var input = form.getApprovalComments();
    input.setInputValue(TEST_STRING);

    form.setApprovalComments(input);
    form.setProjectClosedOut(YesNo.YES);
    return form;
  }

  private ScapDetail getScapDetail() {
    var scapDetail = new ScapDetail();
    scapDetail.setVersionNumber(1);
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);

    scap.setOrganisationGroupId(ORG_GROUP_ID);
    scapDetail.setScap(scap);

    return scapDetail;
  }

  private List<ScapDetail> getScapDetails() {
    var list = new ArrayList<ScapDetail>();
    var scapDetail = new ScapDetail();
    scapDetail.setVersionNumber(3);
    scapDetail.setStatus(ScapDetailStatus.DRAFT);
    scapDetail.setScap(scap);
    scapDetail.setCreatedTimestamp(Instant.now());
    list.add(scapDetail);

    list.add(getScapDetail());

    var scapDetail3 = new ScapDetail();
    scapDetail3.setVersionNumber(1);
    scapDetail3.setStatus(ScapDetailStatus.WITHDRAWN);
    scapDetail.setScap(scap);
    scapDetail3.setCreatedTimestamp(Instant.now());
    list.add(scapDetail3);

    return list;
  }

  private OrganisationGroup getOrgGroup() {
    var orgGroup = new OrganisationGroup();
    return orgGroup;
  }
}
