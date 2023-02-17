package uk.co.nstauthority.scap.notify;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Collections;
import java.util.List;
import java.util.Map;
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
import uk.co.fivium.energyportalapi.generated.types.User;
import uk.co.nstauthority.scap.TestEntityProvider;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
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

  @InjectMocks
  private ScapEmailService scapEmailService;

  @Captor
  ArgumentCaptor<EmailProperties> argumentCaptor;

  private static final ScapDetail SCAP_DETAIL = TestEntityProvider.getScapDetail();
  private static final ServiceUserDetail USER = TestUserProvider.getUser();
  private static final ScapSubmissionStage SCAP_SUBMISSION_STAGE = ScapSubmissionStage.CONTRACTING_PERFORMANCE;
  private static final String CONTEXT_PATH = "/scap";
  private static final String SERVICE_URL = "http://localhost" + CONTEXT_PATH;

  private User recipient;
  private Team team;
  private TeamMember teamMember1;
  private TeamMember teamMember2;

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
    team = new Team();
    teamMember1 = new TeamMember(
        new WebUserAccountId(Long.valueOf(recipient.getWebUserAccountId())),
        null,
        Collections.singleton(IndustryTeamRole.SCAP_SUBMITTER)
    );
    teamMember2 = new TeamMember(
        new WebUserAccountId(23L), null, Collections.singleton(IndustryTeamRole.SCAP_VIEWER)
    );

    when(teamService.getByEnergyPortalOrgGroupId(SCAP_DETAIL.getScap().getOrganisationGroupId()))
        .thenReturn(team);
    when(teamMemberService.getTeamMembers(team)).thenReturn(List.of(teamMember1, teamMember2));
    when(energyPortalUserService.searchUsersByIds(Collections.singletonList(teamMember1.wuaId())))
        .thenReturn(Collections.singletonList(recipient));
  }

  @Test
  void sendScapApprovalEmails() {
    var expectedScapCaseUrl = SERVICE_URL +
        ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(SCAP_DETAIL.getScap().getScapId()));

    scapEmailService.sendScapApprovalEmails(SCAP_DETAIL, USER, SCAP_SUBMISSION_STAGE);

    verify(notifyEmailService).sendEmail(argumentCaptor.capture(), eq(recipient.getPrimaryEmailAddress()));

    assertThat(argumentCaptor.getValue()).extracting(
        EmailProperties::getTemplate,
        EmailProperties::getEmailPersonalisations
    ).containsExactly(
        NotifyTemplate.SCAP_APPROVED,
        Map.of(
            "SCAP reference", SCAP_DETAIL.getScap().getReference(),
            "SCAP submission stage", SCAP_SUBMISSION_STAGE.getDisplayName(),
            "approving user name", USER.forename(),
            "SCAP case url", expectedScapCaseUrl,
            "recipient name", recipient.getForename(),
            "TEST_EMAIL", "no"
        )
    );
  }
}
