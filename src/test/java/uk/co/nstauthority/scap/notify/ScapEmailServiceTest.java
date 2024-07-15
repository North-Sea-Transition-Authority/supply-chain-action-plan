package uk.co.nstauthority.scap.notify;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.tuple;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import uk.co.fivium.digitalnotificationlibrary.core.notification.DomainReference;
import uk.co.fivium.digitalnotificationlibrary.core.notification.MailMergeField;
import uk.co.fivium.digitalnotificationlibrary.core.notification.MergedTemplate;
import uk.co.fivium.digitalnotificationlibrary.core.notification.NotificationLibraryClient;
import uk.co.fivium.digitalnotificationlibrary.core.notification.Template;
import uk.co.fivium.digitalnotificationlibrary.core.notification.TemplateType;
import uk.co.fivium.digitalnotificationlibrary.core.notification.email.EmailRecipient;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.fivium.energyportalapi.generated.types.User;
import uk.co.nstauthority.scap.TestEntityProvider;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.correlationidutil.CorrelationIdUtil;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;

// Setting as fields for ease of modifying when adding other emails
@SuppressWarnings("FieldCanBeLocal")
@ExtendWith(MockitoExtension.class)
class ScapEmailServiceTest {

  static MockHttpServletRequest request;

  private static final Template TEMPLATE = new Template(
      "",
      TemplateType.EMAIL,
      Set.of(),
      Template.VerificationStatus.CONFIRMED_NOTIFY_TEMPLATE
  );

  @Mock
  private NotificationLibraryClient notificationLibraryClient;

  @Mock
  private TeamService teamService;

  @Mock
  private TeamMemberService teamMemberService;

  @Mock
  private EnergyPortalUserService energyPortalUserService;

  @Mock
  private UserDetailService userDetailService;

  @Mock
  private OrganisationGroupService organisationGroupService;

  @InjectMocks
  private ScapEmailService scapEmailService;

  @Captor
  private ArgumentCaptor<MergedTemplate> mergedTemplateCaptor;

  @Captor
  private ArgumentCaptor<EmailRecipient> emailRecipientCaptor;

  @Captor
  private ArgumentCaptor<DomainReference> domainReferenceCaptor;

  private static final ScapDetail SCAP_DETAIL = TestEntityProvider.getScapDetail();
  private static final ServiceUserDetail LOGGED_IN_USER = TestUserProvider.getUser();
  private static final ScapSubmissionStage SCAP_SUBMISSION_STAGE = ScapSubmissionStage.CONTRACTING_PERFORMANCE;
  private static final String CONTEXT_PATH = "/scap";
  private static final String SERVICE_URL = "http://localhost" + CONTEXT_PATH;
  private static final String SCAP_SUMMARY_URL = SERVICE_URL +
      ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_DETAIL.getScap().getScapId()));

  private User recipient;
  private Team industryTeam;
  private TeamMember industryTeamMember1;
  private TeamMember industryTeamMember2;
  private OrganisationGroup organisationGroup;
  private Team regulatorTeam;
  private TeamMember regulatorTeamMember1;
  private TeamMember regulatorTeamMember2;

  @BeforeAll
  static void beforeAll() {
    request = new MockHttpServletRequest();
    request.setContextPath(CONTEXT_PATH);
    RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));
  }

  @BeforeEach
  void beforeEach() {
    recipient = User.newBuilder()
        .forename("Jim")
        .primaryEmailAddress("jim@scap.co.uk")
        .webUserAccountId(1)
        .build();
    industryTeam = new Team();
    industryTeamMember1 = new TeamMember(
        new WebUserAccountId(Long.valueOf(recipient.getWebUserAccountId())),
        null,
        Collections.singleton(IndustryTeamRole.SCAP_SUBMITTER)
    );
    industryTeamMember2 = new TeamMember(
        new WebUserAccountId(23L), null, Collections.singleton(IndustryTeamRole.SCAP_VIEWER)
    );

    regulatorTeam = new Team();
    regulatorTeamMember1 = new TeamMember(
        new WebUserAccountId(Long.valueOf(recipient.getWebUserAccountId())),
        null,
        Collections.singleton(RegulatorTeamRole.SCAP_CASE_OFFICER));
    regulatorTeamMember2 = new TeamMember(
        new WebUserAccountId(7L),
        null,
        Collections.singleton(RegulatorTeamRole.SCAP_VIEWER));

    organisationGroup = OrganisationGroup.newBuilder().name("CENTRICA").build();

    when(userDetailService.getUserDetail()).thenReturn(LOGGED_IN_USER);
  }

  @ParameterizedTest
  @MethodSource("projectClosedOutArguments")
  void sendScapApprovalEmails(boolean projectClosedOutBool, String projectClosedOutString) {
    when(notificationLibraryClient.getTemplate(GovukNotifyTemplate.SCAP_APPROVAL_NOTIFICATION.getTemplateId()))
        .thenReturn(TEMPLATE);
    when(teamService.getByEnergyPortalOrgGroupId(SCAP_DETAIL.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);
    when(teamMemberService.getTeamMembers(industryTeam)).thenReturn(List.of(industryTeamMember1, industryTeamMember2));
    when(energyPortalUserService.searchUsersByIds(Collections.singletonList(industryTeamMember1.wuaId())))
        .thenReturn(Collections.singletonList(recipient));

    CorrelationIdUtil.setCorrelationIdOnMdc("log-correlation-id");

    scapEmailService.sendScapApprovalEmails(SCAP_DETAIL, SCAP_SUBMISSION_STAGE, projectClosedOutBool);

    verify(notificationLibraryClient).sendEmail(
        mergedTemplateCaptor.capture(),
        emailRecipientCaptor.capture(),
        domainReferenceCaptor.capture(),
        eq("log-correlation-id")
    );

    assertThat(mergedTemplateCaptor.getValue().getTemplate()).isEqualTo(TEMPLATE);
    assertThat(mergedTemplateCaptor.getValue().getMailMergeFields())
        .extracting(MailMergeField::name, MailMergeField::value)
        .containsOnly(
            tuple("SCAP reference", SCAP_DETAIL.getScap().getReference()),
            tuple("SCAP submission stage", SCAP_SUBMISSION_STAGE.getDisplayName()),
            tuple("action-performing user", LOGGED_IN_USER.displayName()),
            tuple("SCAP case url", SCAP_SUMMARY_URL),
            tuple("recipient name", recipient.getForename()),
            tuple("Project closed out", projectClosedOutString)
        );

    assertThat(emailRecipientCaptor.getValue().getEmailAddress()).isEqualTo(recipient.getPrimaryEmailAddress());

    assertThat(domainReferenceCaptor.getValue()).isEqualTo(SCAP_DETAIL);
  }

  @Test
  void sendScapWithdrawalEmails() {
    when(notificationLibraryClient.getTemplate(GovukNotifyTemplate.SCAP_WITHDRAWAL_NOTIFICATION.getTemplateId()))
        .thenReturn(TEMPLATE);
    when(teamService.getByEnergyPortalOrgGroupId(SCAP_DETAIL.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);
    when(teamMemberService.getTeamMembers(industryTeam)).thenReturn(List.of(industryTeamMember1, industryTeamMember2));
    when(energyPortalUserService.searchUsersByIds(Collections.singletonList(industryTeamMember1.wuaId())))
        .thenReturn(Collections.singletonList(recipient));

    CorrelationIdUtil.setCorrelationIdOnMdc("log-correlation-id");

    scapEmailService.sendScapWithdrawalEmails(SCAP_DETAIL);

    verify(notificationLibraryClient).sendEmail(
        mergedTemplateCaptor.capture(),
        emailRecipientCaptor.capture(),
        domainReferenceCaptor.capture(),
        eq("log-correlation-id")
    );

    assertThat(mergedTemplateCaptor.getValue().getTemplate()).isEqualTo(TEMPLATE);
    assertThat(mergedTemplateCaptor.getValue().getMailMergeFields())
        .extracting(MailMergeField::name, MailMergeField::value)
        .containsOnly(
            tuple("SCAP reference", SCAP_DETAIL.getScap().getReference()),
            tuple("action-performing user", LOGGED_IN_USER.displayName()),
            tuple("SCAP case url", SCAP_SUMMARY_URL),
            tuple("recipient name", recipient.getForename())
        );

    assertThat(emailRecipientCaptor.getValue().getEmailAddress()).isEqualTo(recipient.getPrimaryEmailAddress());

    assertThat(domainReferenceCaptor.getValue()).isEqualTo(SCAP_DETAIL);
  }

  @Test
  void sendScapSubmissionEmails() {
    when(notificationLibraryClient.getTemplate(GovukNotifyTemplate.SCAP_SUBMISSION_NOTIFICATION.getTemplateId()))
        .thenReturn(TEMPLATE);
    when(teamService.getRegulatorTeam()).thenReturn(regulatorTeam);
    when(teamMemberService.getTeamMembers(regulatorTeam)).thenReturn(List.of(regulatorTeamMember1, regulatorTeamMember2));
    when(energyPortalUserService.searchUsersByIds(Collections.singletonList(regulatorTeamMember1.wuaId())))
        .thenReturn(Collections.singletonList(recipient));
    when(organisationGroupService.getOrganisationGroupById(
        SCAP_DETAIL.getScap().getOrganisationGroupId(), ScapEmailService.SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE))
        .thenReturn(Optional.of(organisationGroup));

    CorrelationIdUtil.setCorrelationIdOnMdc("log-correlation-id");

    scapEmailService.sendScapSubmissionEmails(SCAP_DETAIL);

    verify(notificationLibraryClient).sendEmail(
        mergedTemplateCaptor.capture(),
        emailRecipientCaptor.capture(),
        domainReferenceCaptor.capture(),
        eq("log-correlation-id")
    );

    assertThat(mergedTemplateCaptor.getValue().getTemplate()).isEqualTo(TEMPLATE);
    assertThat(mergedTemplateCaptor.getValue().getMailMergeFields())
        .extracting(MailMergeField::name, MailMergeField::value)
        .containsOnly(
            tuple("SCAP reference", SCAP_DETAIL.getScap().getReference()),
            tuple(  "action-performing user", LOGGED_IN_USER.displayName()),
            tuple("SCAP case url", SCAP_SUMMARY_URL),
            tuple("recipient name", recipient.getForename()),
            tuple("organisation name", organisationGroup.getName())
        );

    assertThat(emailRecipientCaptor.getValue().getEmailAddress()).isEqualTo(recipient.getPrimaryEmailAddress());

    assertThat(domainReferenceCaptor.getValue()).isEqualTo(SCAP_DETAIL);
  }

  @Test
  void sendScapSubmissionEmails_OrgGroupNotFound_AssertThrows() {
    when(notificationLibraryClient.getTemplate(GovukNotifyTemplate.SCAP_SUBMISSION_NOTIFICATION.getTemplateId()))
        .thenReturn(TEMPLATE);

    var organisationGroupId = SCAP_DETAIL.getScap().getOrganisationGroupId();

    when(organisationGroupService.getOrganisationGroupById(
        organisationGroupId, ScapEmailService.SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE))
        .thenReturn(Optional.empty());

    CorrelationIdUtil.setCorrelationIdOnMdc("log-correlation-id");

    assertThatThrownBy(() -> scapEmailService.sendScapSubmissionEmails(SCAP_DETAIL))
        .isInstanceOf(ScapEntityNotFoundException.class)
        .hasMessage("Could not find Organisation Group with ID [%d]".formatted(organisationGroupId));

    verify(organisationGroupService).getOrganisationGroupById(
        organisationGroupId, ScapEmailService.SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE
    );
    verifyNoMoreInteractions(
        notificationLibraryClient, teamService, teamMemberService, energyPortalUserService, organisationGroupService
    );
  }

  private static Stream<Arguments> projectClosedOutArguments() {
    return Stream.of(
        Arguments.of(true, "yes"),
        Arguments.of(false, "no")
    );
  }
}
