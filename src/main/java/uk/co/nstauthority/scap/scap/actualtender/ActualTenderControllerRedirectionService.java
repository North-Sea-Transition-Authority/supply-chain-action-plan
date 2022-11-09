package uk.co.nstauthority.scap.scap.actualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Set;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;

@Service
public class ActualTenderControllerRedirectionService {

  private static final Set<ContractStage> BID_PARTICIPANT_ALLOWED_STAGES =
      Set.of(ContractStage.CONTRACT_AWARDED, ContractStage.INVITATION_TO_TENDER);

  public ModelAndView redirectFromActualTenderActivityForm(Integer scapId, ActualTenderActivity actualTenderDetail) {
    if (BID_PARTICIPANT_ALLOWED_STAGES.contains(actualTenderDetail.getContractStage())) {
      return ReverseRouter.redirect(on(BidParticipantsController.class)
          .renderBidParticipantsForm(scapId, actualTenderDetail.getId(), null));
    }
    // TODO SCAP2022-43: Replace with redirect to Actual Tender Activity summary
    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
  }

  public ModelAndView redirectFromBidParticipantsForm(Integer scapId) {
    // TODO SCAP2022-43: Check if status is CONTRACT_AWARDED, redirect to relevant form page if it is
    return ReverseRouter.redirect(on(TaskListController.class).renderTaskList(scapId));
  }
}
