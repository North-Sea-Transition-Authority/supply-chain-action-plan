package uk.co.nstauthority.scap.notify;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.fivium.energyportalapi.generated.types.User;
import uk.co.nstauthority.scap.TestEntityProvider;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.authentication.UserDetailService;
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

  @Mock
  private NotifyEmailService notifyEmailService;

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
  ArgumentCaptor<EmailProperties> argumentCaptor;

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
  static void setupTests() {
    request = new MockHttpServletRequest();
    request.setContextPath(CONTEXT_PATH);
    RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));
  }

  @BeforeEach
  void setup() {
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

  @Test
  void sendScapApprovalEmails() {
    when(teamService.getByEnergyPortalOrgGroupId(SCAP_DETAIL.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);
    when(teamMemberService.getTeamMembers(industryTeam)).thenReturn(List.of(industryTeamMember1, industryTeamMember2));
    when(energyPortalUserService.searchUsersByIds(Collections.singletonList(industryTeamMember1.wuaId())))
        .thenReturn(Collections.singletonList(recipient));

    scapEmailService.sendScapApprovalEmails(SCAP_DETAIL, SCAP_SUBMISSION_STAGE);

    verify(notifyEmailService).sendEmail(argumentCaptor.capture(), eq(recipient.getPrimaryEmailAddress()));

    assertThat(argumentCaptor.getValue()).extracting(
        EmailProperties::getTemplate,
        EmailProperties::getEmailPersonalisations
    ).containsExactly(
        NotifyTemplate.SCAP_APPROVED,
        Map.of(
            "SCAP reference", SCAP_DETAIL.getScap().getReference(),
            "SCAP submission stage", SCAP_SUBMISSION_STAGE.getDisplayName(),
            "action-performing user", LOGGED_IN_USER.displayName(),
            "SCAP case url", SCAP_SUMMARY_URL,
            "recipient name", recipient.getForename(),
            "TEST_EMAIL", "no"
        )
    );
  }

  @Test
  void sendScapWithdrawalEmails() {
    when(teamService.getByEnergyPortalOrgGroupId(SCAP_DETAIL.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);
    when(teamMemberService.getTeamMembers(industryTeam)).thenReturn(List.of(industryTeamMember1, industryTeamMember2));
    when(energyPortalUserService.searchUsersByIds(Collections.singletonList(industryTeamMember1.wuaId())))
        .thenReturn(Collections.singletonList(recipient));

    scapEmailService.sendScapWithdrawalEmails(SCAP_DETAIL);

    verify(notifyEmailService).sendEmail(argumentCaptor.capture(), eq(recipient.getPrimaryEmailAddress()));

    assertThat(argumentCaptor.getValue()).extracting(
        EmailProperties::getTemplate,
        EmailProperties::getEmailPersonalisations
    ).containsExactly(
        NotifyTemplate.SCAP_WITHDRAWN,
        Map.of(
            "SCAP reference", SCAP_DETAIL.getScap().getReference(),
            "action-performing user", LOGGED_IN_USER.displayName(),
            "SCAP case url", SCAP_SUMMARY_URL,
            "recipient name", recipient.getForename(),
            "TEST_EMAIL", "no"
        )
    );
  }

  @Test
  void sendScapSubmissionEmails() {
    when(teamService.getRegulatorTeam()).thenReturn(regulatorTeam);
    when(teamMemberService.getTeamMembers(regulatorTeam)).thenReturn(List.of(regulatorTeamMember1, regulatorTeamMember2));
    when(energyPortalUserService.searchUsersByIds(Collections.singletonList(regulatorTeamMember1.wuaId())))
        .thenReturn(Collections.singletonList(recipient));
    when(organisationGroupService.getOrganisationGroupById(
        SCAP_DETAIL.getScap().getOrganisationGroupId(), ScapEmailService.SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE))
        .thenReturn(Optional.of(organisationGroup));

    scapEmailService.sendScapSubmissionEmails(SCAP_DETAIL);

    verify(notifyEmailService).sendEmail(argumentCaptor.capture(), eq(recipient.getPrimaryEmailAddress()));

    assertThat(argumentCaptor.getValue()).extracting(
        EmailProperties::getTemplate,
        EmailProperties::getEmailPersonalisations
    ).containsExactly(
        NotifyTemplate.SCAP_SUBMITTED,
        Map.of(
            "SCAP reference", SCAP_DETAIL.getScap().getReference(),
            "action-performing user", LOGGED_IN_USER.displayName(),
            "SCAP case url", SCAP_SUMMARY_URL,
            "recipient name", recipient.getForename(),
            "TEST_EMAIL", "no",
            "organisation name", organisationGroup.getName()
        )
    );
  }

  @Test
  void sendScapSubmissionEmails_OrgGroupNotFound_AssertThrows() {
    var organisationGroupId = SCAP_DETAIL.getScap().getOrganisationGroupId();

    when(organisationGroupService.getOrganisationGroupById(
        organisationGroupId, ScapEmailService.SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE))
        .thenReturn(Optional.empty());

    assertThatThrownBy(() -> scapEmailService.sendScapSubmissionEmails(SCAP_DETAIL))
        .isInstanceOf(ScapEntityNotFoundException.class)
        .hasMessage("Could not find Organisation Group with ID [%d]".formatted(organisationGroupId));

    verify(organisationGroupService).getOrganisationGroupById(
        organisationGroupId, ScapEmailService.SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE
    );
    verifyNoMoreInteractions(
        notifyEmailService, teamService, teamMemberService, energyPortalUserService, organisationGroupService
    );
  }
}
