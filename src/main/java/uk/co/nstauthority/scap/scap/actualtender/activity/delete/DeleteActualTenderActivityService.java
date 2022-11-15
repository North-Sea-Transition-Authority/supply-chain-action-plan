package uk.co.nstauthority.scap.scap.actualtender.activity.delete;

import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.util.NotificationBannerUtils;

@Service
class DeleteActualTenderActivityService {

  private final ActualTenderActivityService actualTenderActivityService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractService awardedContractService;

  @Autowired
  DeleteActualTenderActivityService(ActualTenderActivityService actualTenderActivityService,
                                    InvitationToTenderParticipantService invitationToTenderParticipantService,
                                    AwardedContractService awardedContractService) {
    this.actualTenderActivityService = actualTenderActivityService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractService = awardedContractService;
  }

  @Transactional
  void deleteActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    awardedContractService.deleteByActualTenderActivity(actualTenderActivity);
    invitationToTenderParticipantService.deleteAllByActualTenderActivity(actualTenderActivity);
    actualTenderActivityService.deleteActualTenderActivity(actualTenderActivity);
  }

  void addActualTenderDeletionSuccessBanner(RedirectAttributes redirectAttributes, String scopeTitle) {
    NotificationBannerUtils.successBannerRedirect(
        "Success",
        new NotificationBannerBodyLine(
            "%s has been removed from this SCAP".formatted(scopeTitle), "govuk-!-font-weight-bold"
        ), redirectAttributes);
  }
}
