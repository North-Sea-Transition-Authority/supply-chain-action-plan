package uk.co.nstauthority.scap.scap.summary.actualtender;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Map;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.delete.DeleteActualTenderActivityController;
import uk.co.nstauthority.scap.scap.scap.ScapId;

public record ActualTenderActivitySummaryView(ScapId scapId,
                                              Integer activityId,
                                              String scopeTitle,
                                              String scopeDescription,
                                              RemunerationModel remunerationModel,
                                              String remunerationModelName,
                                              ContractStage contractStage,
                                              Map<String, Boolean> ittParticipants,
                                              Map<String, Boolean> bidParticipants,
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
