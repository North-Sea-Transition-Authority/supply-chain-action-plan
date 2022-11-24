package uk.co.nstauthority.scap.scap.actualtender.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.delete.DeleteActualTenderActivityController;

public record ActualTenderSummaryView(Integer scapId,
                                      Integer activityId,
                                      String scopeTitle,
                                      String scopeDescription,
                                      RemunerationModel remunerationModel,
                                      String remunerationModelName,
                                      ContractStage contractStage,
                                      List<String> invitationToTenderParticipants,
                                      List<String> bidParticipants,
                                      AwardedContractSummaryView awardedContract) {

  public String getChangeLinkUrl() {
    return ReverseRouter.route(on(ActualTenderActivityController.class)
        .renderExistingActualTenderActivityForm(scapId, activityId));
  }

  public String getDeleteLinkUrl() {
    return ReverseRouter.route(on(DeleteActualTenderActivityController.class)
        .renderDeleteActualTenderActivityConfirmation(scapId, activityId));
  }

}
