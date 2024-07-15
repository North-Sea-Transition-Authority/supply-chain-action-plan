package uk.co.nstauthority.scap.notify;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.digitalnotificationlibrary.core.notification.DomainReference;
import uk.co.fivium.digitalnotificationlibrary.core.notification.MergedTemplate;
import uk.co.fivium.digitalnotificationlibrary.core.notification.NotificationLibraryClient;
import uk.co.fivium.digitalnotificationlibrary.core.notification.email.EmailRecipient;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.fivium.energyportalapi.generated.types.User;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.correlationidutil.CorrelationIdUtil;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.summary.ScapSubmissionStage;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.util.AbsoluteReverseRouter;

@Service
public class ScapEmailService {

  private final NotificationLibraryClient notificationLibraryClient;
  private final TeamService teamService;
  private final TeamMemberService teamMemberService;
  private final EnergyPortalUserService energyPortalUserService;
  private final UserDetailService userDetailService;
  private final OrganisationGroupService organisationGroupService;

  static final String SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE =
      "Get organisation name for sending SCAP submission confirmation.";

  @Autowired
  ScapEmailService(
      NotificationLibraryClient notificationLibraryClient,
      TeamService teamService,
      TeamMemberService teamMemberService,
      EnergyPortalUserService energyPortalUserService,
      UserDetailService userDetailService,
      OrganisationGroupService organisationGroupService
  ) {
    this.notificationLibraryClient = notificationLibraryClient;
    this.teamService = teamService;
    this.teamMemberService = teamMemberService;
    this.energyPortalUserService = energyPortalUserService;
    this.userDetailService = userDetailService;
    this.organisationGroupService = organisationGroupService;
  }

  public void sendScapApprovalEmails(
      ScapDetail scapDetail,
      ScapSubmissionStage scapSubmissionStage,
      boolean projectClosedOut
  ) {
    var scap = scapDetail.getScap();

    var mergedTemplateBuilder = getTemplateForScap(GovukNotifyTemplate.SCAP_APPROVAL_NOTIFICATION, scap)
        .withMailMergeField("SCAP submission stage", scapSubmissionStage.getDisplayName())
        .withMailMergeField("Project closed out", projectClosedOut ? "yes" : "no");

    var recipients = getScapSubmitterRecipients(scap);
    sendEmailsWithRecipientNames(mergedTemplateBuilder, recipients, scapDetail);
  }

  public void sendScapWithdrawalEmails(ScapDetail scapDetail) {
    var scap = scapDetail.getScap();

    var mergedTemplateBuilder = getTemplateForScap(GovukNotifyTemplate.SCAP_WITHDRAWAL_NOTIFICATION, scap);

    var recipients = getScapSubmitterRecipients(scap);
    sendEmailsWithRecipientNames(mergedTemplateBuilder, recipients, scapDetail);
  }

  public void sendScapSubmissionEmails(ScapDetail scapDetail) {
    var scap = scapDetail.getScap();

    var organisationGroup = organisationGroupService.getOrganisationGroupById(
        scap.getOrganisationGroupId(), SUBMISSION_EMAIL_ORG_GROUP_REQUEST_PURPOSE
    );

    var mergedTemplateBuilder = getTemplateForScap(GovukNotifyTemplate.SCAP_SUBMISSION_NOTIFICATION, scap)
        .withMailMergeField(
            "organisation name",
            organisationGroup.map(OrganisationGroup::getName)
                .orElseThrow(() ->
                    new ScapEntityNotFoundException(
                        "Could not find Organisation Group with ID [%d]".formatted(scap.getOrganisationGroupId()))
                )
        );

    var recipients = getScapCaseOfficerRecipients();
    sendEmailsWithRecipientNames(mergedTemplateBuilder, recipients, scapDetail);
  }

  private MergedTemplate.MergedTemplateBuilder getTemplateForScap(GovukNotifyTemplate notifyTemplate, Scap scap) {
    var currentUser = userDetailService.getUserDetail();

    return notificationLibraryClient.getTemplate(notifyTemplate.getTemplateId())
        .withMailMergeField("action-performing user", currentUser.displayName())
        .withMailMergeField("SCAP reference", scap.getReference())
        .withMailMergeField(
            "SCAP case url",
            AbsoluteReverseRouter.route(on(ScapSummaryController.class).getScapSummary(scap.getScapId()))
        );
  }

  private List<User> getScapCaseOfficerRecipients() {
    var team = teamService.getRegulatorTeam();
    var regulatorTeamMembers = teamMemberService.getTeamMembers(team);
    var teamMemberIds = regulatorTeamMembers.stream()
        .filter(teamMember -> teamMember.roles().contains(RegulatorTeamRole.SCAP_CASE_OFFICER))
        .map(TeamMember::wuaId).toList();
    return energyPortalUserService.searchUsersByIds(teamMemberIds);
  }

  private List<User> getScapSubmitterRecipients(Scap scap) {
    var team = teamService.getByEnergyPortalOrgGroupId(scap.getOrganisationGroupId());
    var submittingOrgTeamMembers = teamMemberService.getTeamMembers(team);
    var teamMemberIds = submittingOrgTeamMembers.stream()
        .filter(teamMember -> teamMember.roles().contains(IndustryTeamRole.SCAP_SUBMITTER))
        .map(TeamMember::wuaId).toList();
    return energyPortalUserService.searchUsersByIds(teamMemberIds);
  }

  private void sendEmailsWithRecipientNames(
      MergedTemplate.MergedTemplateBuilder mergedTemplateBuilder,
      List<User> recipients,
      DomainReference domainReference
  ) {
    recipients.forEach(recipient -> {
      var mergedTemplate = mergedTemplateBuilder
          .withMailMergeField("recipient name", recipient.getForename())
          .merge();

      var emailRecipient = EmailRecipient.directEmailAddress(recipient.getPrimaryEmailAddress());

      notificationLibraryClient.sendEmail(
          mergedTemplate,
          emailRecipient,
          domainReference,
          CorrelationIdUtil.getCorrelationIdFromMdc()
      );
    });
  }
}
