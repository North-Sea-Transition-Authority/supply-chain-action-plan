package uk.co.nstauthority.scap.scap.actualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Set;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractController;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsController;
import uk.co.nstauthority.scap.scap.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.actualtender.summary.HasMoreActualTenderActivities;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Service
public class ActualTenderControllerRedirectionService {

  private static final Set<ContractStage> BID_PARTICIPANT_ALLOWED_STAGES =
      Set.of(ContractStage.CONTRACT_AWARDED, ContractStage.BID_APPRAISAL);

  public ModelAndView redirectFromActualTenderActivityForm(ScapId scapId, ActualTenderActivity actualTenderDetail) {
    if (BID_PARTICIPANT_ALLOWED_STAGES.contains(actualTenderDetail.getContractStage())) {
      return ReverseRouter.redirect(on(BidParticipantsController.class)
          .renderBidParticipantsForm(scapId, actualTenderDetail.getId(), null));
    }
    return ReverseRouter.redirect(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));
  }

  public ModelAndView redirectFromBidParticipantsForm(ScapId scapId, ActualTenderActivity actualTenderActivity) {
    if (ContractStage.CONTRACT_AWARDED.equals(actualTenderActivity.getContractStage())) {
      return ReverseRouter.redirect(on(AwardedContractController.class)
          .renderAwardedContractForm(scapId, actualTenderActivity.getId()));
    }
    return ReverseRouter.redirect(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));
  }

  public ModelAndView redirectFromActualTenderDeletion(ScapId scapId, Boolean hasActualTenderDetails) {
    if (Boolean.TRUE.equals(hasActualTenderDetails)) {
      return ReverseRouter.redirect(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));
    }

    return ReverseRouter.redirect(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));
  }

  public ModelAndView redirectFromActualTenderSummary(ScapId scapId,
                                                      HasMoreActualTenderActivities hasMoreActualTenderActivities) {
    if (HasMoreActualTenderActivities.YES_NOW.equals(hasMoreActualTenderActivities)) {
      return ReverseRouter.redirect(on(ActualTenderActivityController.class).renderActualTenderActivityForm(scapId, null));
    }

    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
  }
}
