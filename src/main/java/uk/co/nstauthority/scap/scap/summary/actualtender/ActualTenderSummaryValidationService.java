package uk.co.nstauthority.scap.scap.summary.actualtender;

import java.util.List;
import java.util.Objects;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.actualtender.ActualTender;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityFormService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ContractStage;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContract;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractFormService;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsForm;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsFormService;

@Service
class ActualTenderSummaryValidationService {

  private final ActualTenderActivityFormService actualTenderActivityFormService;
  private final BidParticipantsFormService bidParticipantsFormService;
  private final AwardedContractFormService awardedContractFormService;

  @Autowired
  ActualTenderSummaryValidationService(ActualTenderActivityFormService actualTenderActivityFormService,
                                       BidParticipantsFormService bidParticipantsFormService,
                                       AwardedContractFormService awardedContractFormService) {
    this.actualTenderActivityFormService = actualTenderActivityFormService;
    this.bidParticipantsFormService = bidParticipantsFormService;
    this.awardedContractFormService = awardedContractFormService;
  }

  boolean isValid(ActualTender actualTender,
                  ActualTenderActivity actualTenderActivity,
                  List<InvitationToTenderParticipant> ittParticipants,
                  List<InvitationToTenderParticipant> bidParticipants,
                  AwardedContract awardedContract) {
    var actualTenderActivityFormIsValid = actualTenderActivityFormIsValid(actualTender, actualTenderActivity, ittParticipants);

    if (!(actualTenderActivityFormIsValid)
        || ContractStage.INVITATION_TO_TENDER_IS_LIVE.equals(actualTenderActivity.getContractStage())
    ) {
      return actualTenderActivityFormIsValid;
    }

    var bidParticipantsFormIsValid = bidParticipantsFormIsValid(bidParticipants);

    if (!(bidParticipantsFormIsValid)
        || ContractStage.BID_APPRAISAL.equals(actualTenderActivity.getContractStage())) {
      return bidParticipantsFormIsValid;
    }

    return awardedContractFormIsValid(awardedContract, bidParticipants);
  }

  private boolean actualTenderActivityFormIsValid(ActualTender actualTender,
                                                  ActualTenderActivity actualTenderActivity,
                                                  List<InvitationToTenderParticipant> participants) {
    var form = actualTenderActivityFormService.getForm(actualTenderActivity, participants);
    var bindingResult = getBindingResult(form);
    bindingResult = actualTenderActivityFormService.validate(
        form,
        bindingResult,
        actualTender,
        actualTenderActivity
    );
    return !bindingResult.hasErrors();
  }

  private boolean bidParticipantsFormIsValid(List<InvitationToTenderParticipant> bidParticipants) {
    var form = new BidParticipantsForm();
    form.setSelectedBidParticipantIds(BidParticipantsFormService.getParticipantIds(bidParticipants));
    var bindingResult = getBindingResult(form);
    bindingResult = bidParticipantsFormService.validate(form, bindingResult, bidParticipants);
    return !bindingResult.hasErrors();
  }

  private boolean awardedContractFormIsValid(AwardedContract awardedContract,
                                             List<InvitationToTenderParticipant> bidParticipants) {
    if (Objects.isNull(awardedContract)) {
      return false;
    }
    var form = awardedContractFormService.getForm(awardedContract);
    var bindingResult = getBindingResult(form);
    bindingResult = awardedContractFormService.validate(form, bindingResult, bidParticipants);
    return !bindingResult.hasErrors();
  }

  private BindingResult getBindingResult(Object form) {
    return new BeanPropertyBindingResult(form, "form");
  }
}
