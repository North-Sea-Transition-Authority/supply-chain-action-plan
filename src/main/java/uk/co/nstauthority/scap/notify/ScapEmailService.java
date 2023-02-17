package uk.co.nstauthority.scap.notify;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;
import uk.co.fivium.energyportalapi.generated.types.User;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;

@Service
public class ScapEmailService {

  private final NotifyEmailService notifyEmailService;
  private final TeamService teamService;
  private final TeamMemberService teamMemberService;
  private final EnergyPortalUserService energyPortalUserService;
  private final UserDetailService userDetailService;

  @Autowired
  ScapEmailService(NotifyEmailService notifyEmailService,
                   TeamService teamService,
                   TeamMemberService teamMemberService,
                   EnergyPortalUserService energyPortalUserService,
                   UserDetailService userDetailService) {
    this.notifyEmailService = notifyEmailService;
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.energyPortalUserService = energyPortalUserService;
    this.userDetailService = userDetailService;
  }

  public void sendScapApprovalEmails(ScapDetail scapDetail,
                                     ScapSubmissionStage scapSubmissionStage) {
    var scap = scapDetail.getScap();
    var emailProperties = new EmailProperties(NotifyTemplate.SCAP_APPROVED);
    var personalisations = emailProperties.getEmailPersonalisations();
    addDefaultPersonalisations(personalisations, scap);
    personalisations.put("SCAP submission stage", scapSubmissionStage.getDisplayName());
    var recipients = getScapSubmitterRecipients(scap);
    sendEmailsWithRecipientNames(emailProperties, recipients);
  }

  public void sendScapWithdrawalEmails(ScapDetail scapDetail) {
    var scap = scapDetail.getScap();
    var emailProperties = new EmailProperties(NotifyTemplate.SCAP_WITHDRAWN);
    var personalisations = emailProperties.getEmailPersonalisations();
    addDefaultPersonalisations(personalisations, scap);

    var recipients = getScapSubmitterRecipients(scap);
    sendEmailsWithRecipientNames(emailProperties, recipients);
  }

  private void addDefaultPersonalisations(Map<String, String> personalisations, Scap scap) {
    var regulatorUser = userDetailService.getUserDetail();

    personalisations.put("regulator user name", regulatorUser.displayName());
    personalisations.put("SCAP reference", scap.getReference());
    personalisations.put("SCAP case url", getScapCaseUrl(scap.getScapId()));
  }

  private List<User> getScapSubmitterRecipients(Scap scap) {
    var team = teamService.getByEnergyPortalOrgGroupId(scap.getOrganisationGroupId());
    var submittingOrgTeamMembers = teamMemberService.getTeamMembers(team);
    var teamMemberIds = submittingOrgTeamMembers.stream()
        .filter(teamMember -> teamMember.roles().contains(IndustryTeamRole.SCAP_SUBMITTER))
        .map(TeamMember::wuaId).toList();
    return energyPortalUserService.searchUsersByIds(teamMemberIds);
  }

  private void sendEmailsWithRecipientNames(EmailProperties emailProperties, List<User> recipients) {
    recipients
        .forEach(recipient -> {
          emailProperties.getEmailPersonalisations().put("recipient name", recipient.getForename());
          notifyEmailService.sendEmail(emailProperties, recipient.getPrimaryEmailAddress());
        });
  }

  private String getScapCaseUrl(ScapId scapId) {
    var baseUrl = ServletUriComponentsBuilder.fromCurrentContextPath().toUriString();
    var scapSummaryUrl = ReverseRouter.route(on(ScapSummaryController.class).getScapSummary(scapId));
    return "%s%s".formatted(baseUrl, scapSummaryUrl);
  }
}
