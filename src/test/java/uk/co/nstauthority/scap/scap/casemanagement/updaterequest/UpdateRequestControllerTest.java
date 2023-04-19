package uk.co.nstauthority.scap.scap.casemanagement.updaterequest;

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
import static uk.co.nstauthority.scap.scap.summary.ScapSummaryControllerTestUtil.getScapSummaryView;

import java.time.LocalDate;
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
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = UpdateRequestController.class)
class UpdateRequestControllerTest extends AbstractControllerTest {

  @MockBean
  private CaseEventService caseEventService;

  @SpyBean
  private ControllerHelperService controllerHelperService;

  @MockBean
  private ScapSummaryViewService scapSummaryViewService;

  @MockBean
  private UpdateRequestService updateRequestService;

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private UpdateRequestFormValidator updateRequestFormValidator;

  @MockBean
  private CaseEventDocumentService caseEventDocumentService;

  private static final ScapId SCAP_ID = new ScapId(1111);

  private static final ScapDetail SCAP_DETAIL = getScapDetail();

  private static final Integer ORG_GROUP_ID = 1000;

  private static final OrganisationGroup ORG_GROUP = new OrganisationGroup();

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
    when(scapService.getScapById(anyInt())).thenReturn(new Scap());
    when(scapDetailService.getActionableScapDetail(SCAP_ID, testUser)).thenReturn(SCAP_DETAIL);
    when(scapDetailService.getLatestByScap(any(Scap.class))).thenReturn(SCAP_DETAIL);
    when(organisationGroupService.getOrganisationGroupById(eq(ORG_GROUP_ID), any())).thenReturn(Optional.of(ORG_GROUP));
    when(scapSummaryViewService.getScapSummaryView(SCAP_DETAIL)).thenReturn(getScapSummaryView());
  }

  @Test
  void saveQaComments_ValidationSuccesful_savedWithComments() throws Exception {
    mockMvc.perform(post(ReverseRouter.route(on(UpdateRequestController.class)
            .saveInfoRequestedForm(
                SCAP_ID,
                CaseEventAction.INFO_REQUESTED,
                false,
                getInfoRequestedForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("updateRequestForm", getInfoRequestedForm()))
        .andExpect(status().is3xxRedirection())
        .andExpect(flash().attributeExists("notificationBannerView"));
    verify(caseEventService).recordNewEvent(CaseEventSubject.SCAP_UPDATE_REQUESTED, SCAP_DETAIL, 1, TEST_STRING);
  }

  @Test
  void saveQaComments_ValidationFailed_Reroute() throws Exception {
    doAnswer(invocation -> {
      var bindingResult = (BindingResult) invocation.getArgument(1);
      bindingResult.rejectValue("infoRequest.inputValue", "testError", "This is an error message");
      return bindingResult;
    }).when(updateRequestFormValidator).validate(any(), any());

    mockMvc.perform(post(ReverseRouter.route(on(UpdateRequestController.class)
            .saveInfoRequestedForm(
                SCAP_ID,
                CaseEventAction.INFO_REQUESTED,
                false,
                getInfoRequestedForm(),
                null,
                null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("infoRequestForm", getInfoRequestedForm()))
        .andExpect(status().isOk());

    verify(caseEventService, never()).recordNewEvent(CaseEventSubject.SCAP_UPDATE_REQUESTED, SCAP_DETAIL, 1, TEST_STRING);
  }

  private UpdateRequestForm getInfoRequestedForm() {
    var form = new UpdateRequestForm();
    var input = form.getInfoRequest();
    input.setInputValue(TEST_STRING);

    var dateInput = form.getDueDate();
    dateInput.setDate(LocalDate.now().plusDays(5L));

    form.setInfoRequest(input);
    return form;
  }

  private static ScapDetail getScapDetail() {
    var scapDetail = new ScapDetail(1234);
    scapDetail.setVersionNumber(1);
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);

    var scap = new Scap(SCAP_ID);
    scap.setOrganisationGroupId(ORG_GROUP_ID);
    scapDetail.setScap(scap);

    return scapDetail;
  }
}
