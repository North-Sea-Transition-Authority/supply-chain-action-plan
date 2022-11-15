package uk.co.nstauthority.scap.scap.actualtender.summary;

import java.util.List;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;

public record ActualTenderSummaryView(String scopeTitle,
                                      String scopeDescription,
                                      RemunerationModel remunerationModel,
                                      String remunerationModelName,
                                      ContractStage contractStage,
                                      List<String> invitationToTenderParticipants,
                                      List<String> bidParticipants,
                                      AwardedContractSummaryView awardedContract,
                                      String changeLinkUrl,
                                      String deleteLinkUrl) {

}
