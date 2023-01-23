package uk.co.nstauthority.scap.scap.casemanagement.qacomments;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.authentication.TestUserProvider.user;
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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;

@ExtendWith(MockitoExtension.class)
@WithMockUser
@ContextConfiguration(classes = QaCommentController.class)
class QaCommentControllerTest extends AbstractControllerTest {

  @MockBean
  private CaseEventService caseEventService;

  @SpyBean
  private ControllerHelperService controllerHelperService;

  @MockBean
  private ScapSummaryViewService scapSummaryViewService;

  @MockBean
  private OrganisationGroupService organisationGroupService;

  @MockBean
  private QaCommentFormValidator qaCommentFormValidator;

  private static final ScapId SCAP_ID = new ScapId(1111);

  private static final Integer ORG_GROUP_ID = 1000;

  private static final String TEST_STRING = "This is a test comment";

  @BeforeEach
  void setup() {
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.getAllPermissionsForUser(testUser)).thenReturn(List.of(RolePermission.values()));
    when(scapService.getScapById(anyInt())).thenReturn(new Scap());
    when(scapDetailService.getLatestScapDetailByScapIdOrThrow(SCAP_ID)).thenReturn(getScapDetail());
    when(scapDetailService.getLatestScapDetailByScapOrThrow(any(Scap.class))).thenReturn(getScapDetail());
    when(organisationGroupService.getOrganisationGroupById(eq(ORG_GROUP_ID), any())).thenReturn(Optional.of(getOrgGroup()));
    when(scapSummaryViewService.getScapSummaryView(any())).thenReturn(getScapSummaryView());
  }

  @Test
  void saveQaComments_ValidationSuccesful_saved() throws Exception {
    mockMvc.perform(post(ReverseRouter.route(on(QaCommentController.class)
        .saveQaCommentForm(
            SCAP_ID,
            CaseEventAction.QA,
            false,
            null,
            null)))
        .with(user(testUser))
        .with(csrf()))
        .andExpect(status().is3xxRedirection());

    verify(caseEventService).recordNewEvent(CaseEventSubject.QA_COMMENT, SCAP_ID, 1, null);
  }

  @Test
  void saveQaComments_ValidationSuccesful_savedWithComments() throws Exception {
    mockMvc.perform(post(ReverseRouter.route(on(QaCommentController.class)
            .saveQaCommentForm(
                SCAP_ID,
                CaseEventAction.QA,
                false,
                getQaCommentForm(),
                null)))
            .with(user(testUser))
            .with(csrf())
            .flashAttr("form", getQaCommentForm()))
        .andExpect(status().is3xxRedirection());

    verify(caseEventService).recordNewEvent(CaseEventSubject.QA_COMMENT, SCAP_ID, 1, TEST_STRING);
  }

  @Test
  void saveQaComments_ValidationFailed_Reroute() throws Exception {
    doAnswer(invocation -> {
      var bindingResult = (BindingResult) invocation.getArgument(1);
      bindingResult.rejectValue("qaComments.inputValue", "testError", "This is an error message");
      return bindingResult;
    }).when(qaCommentFormValidator).validate(any(), any());

    mockMvc.perform(post(ReverseRouter.route(on(QaCommentController.class)
            .saveQaCommentForm(
                SCAP_ID,
                CaseEventAction.INFO_REQUESTED,
                false,
                getQaCommentForm(),
                null)))
            .with(user(testUser))
            .with(csrf())
            .flashAttr("infoRequestForm", getQaCommentForm()))
        .andExpect(status().isOk());

    verify(caseEventService, never()).recordNewEvent(CaseEventSubject.FURTHER_INFO_REQUESTED, SCAP_ID, 1, TEST_STRING);
  }

  private QaCommentForm getQaCommentForm() {
    var form = new QaCommentForm();
    var input = form.getQaComments();
    input.setInputValue(TEST_STRING);

    form.setQaComments(input);
    return form;
  }

  private ScapDetail getScapDetail() {
    var scapDetail = new ScapDetail();
    scapDetail.setVersionNumber(1);
    scapDetail.setStatus(ScapDetailStatus.SUBMITTED);

    var scap = new Scap();
    scap.setOrganisationGroupId(ORG_GROUP_ID);
    scapDetail.setScap(scap);

    return scapDetail;
  }

  private OrganisationGroup getOrgGroup() {
    var orgGroup = new OrganisationGroup();
    return orgGroup;
  }
}
