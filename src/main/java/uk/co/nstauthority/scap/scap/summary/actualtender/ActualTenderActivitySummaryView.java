package uk.co.nstauthority.scap.scap.summary.actualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.delete.DeleteActualTenderActivityController;

public record ActualTenderActivitySummaryView(Integer scapId,
                                              Integer activityId,
                                              String scopeTitle,
                                              String scopeDescription,
                                              RemunerationModel remunerationModel,
                                              String remunerationModelName,
                                              ContractStage contractStage,
                                              List<String> ittParticipants,
                                              List<String> bidParticipants,
                                              AwardedContractSummaryView awardedContractSummaryView) {
  public String getChangeLinkUrl() {
    return ReverseRouter.route(on(ActualTenderActivityController.class)
        .renderExistingActualTenderActivityForm(scapId, activityId));
  }

  public String getDeleteLinkUrl() {
    return ReverseRouter.route(on(DeleteActualTenderActivityController.class)
        .renderDeleteActualTenderActivityConfirmation(scapId, activityId));
  }
}
