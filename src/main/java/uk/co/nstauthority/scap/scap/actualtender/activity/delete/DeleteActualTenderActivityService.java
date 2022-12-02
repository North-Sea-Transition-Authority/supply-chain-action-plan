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
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;
import uk.co.nstauthority.scap.util.NotificationBannerUtils;

@Service
class DeleteActualTenderActivityService {

  private final ActualTenderActivityService actualTenderActivityService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractService awardedContractService;
  private final ContractingPerformanceService contractingPerformanceService;

  @Autowired
  DeleteActualTenderActivityService(ActualTenderActivityService actualTenderActivityService,
                                    InvitationToTenderParticipantService invitationToTenderParticipantService,
                                    AwardedContractService awardedContractService,
                                    ContractingPerformanceService contractingPerformanceService) {
    this.actualTenderActivityService = actualTenderActivityService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractService = awardedContractService;
    this.contractingPerformanceService = contractingPerformanceService;
  }

  @Transactional
  void deleteActualTenderActivity(ActualTenderActivity actualTenderActivity) {
    contractingPerformanceService.deleteByActualTenderActivity(actualTenderActivity);
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
