package uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequiredForScap;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants.BidParticipantsController;
import uk.co.nstauthority.scap.scap.actualtender.summary.ActualTenderSummaryController;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/actual-tender/activity/{activityId}/actual-contract-award")
@PermissionsRequiredForScap(permissions = RolePermission.SUBMIT_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.DRAFT)
public class AwardedContractController {

  private final ScapService scapService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;
  private final AwardedContractFormService awardedContractFormService;
  private final ControllerHelperService controllerHelperService;
  private final AwardedContractService awardedContractService;

  @Autowired
  AwardedContractController(ScapService scapService,
                            ActualTenderActivityService actualTenderActivityService,
                            InvitationToTenderParticipantService invitationToTenderParticipantService,
                            AwardedContractFormService awardedContractFormService,
                            ControllerHelperService controllerHelperService,
                            AwardedContractService awardedContractService) {
    this.scapService = scapService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
    this.awardedContractFormService = awardedContractFormService;
    this.controllerHelperService = controllerHelperService;
    this.awardedContractService = awardedContractService;
  }

  @GetMapping
  public ModelAndView renderAwardedContractForm(@PathVariable("scapId") ScapId scapId,
                                                @PathVariable("activityId") Integer activityId) {
    scapService.getScapById(scapId);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var bidParticipants = invitationToTenderParticipantService
        .getBidParticipants(actualTenderActivity);
    var awardedContract = awardedContractService.getByActualTenderActivity(actualTenderActivity);
    var form = awardedContract.map(awardedContractFormService::getForm)
        .orElse(new AwardedContractForm());
    var preselectedCountry = awardedContract.flatMap(existingAwardedContract ->
        awardedContractFormService.getPreselectedBidderLocation(existingAwardedContract.getPreferredBidderCountryId()))
        .orElse(null);

    return awardedContractModelAndView(scapId, activityId, bidParticipants, preselectedCountry)
        .addObject("form", form);
  }

  @PostMapping
  public ModelAndView saveAwardedContractForm(@PathVariable("scapId") ScapId scapId,
                                              @PathVariable("activityId") Integer activityId,
                                              @ModelAttribute("form") AwardedContractForm form,
                                              BindingResult bindingResult) {
    scapService.getScapById(scapId);
    var actualTenderActivity = actualTenderActivityService.getById(activityId);
    var bidParticipants = invitationToTenderParticipantService
        .getBidParticipants(actualTenderActivity);

    bindingResult = awardedContractFormService.validate(form, bindingResult, bidParticipants);
    Map<String, String> preselectedCountry = null;

    if (bindingResult.hasErrors()) {
      preselectedCountry = awardedContractFormService.getPreselectedBidderLocationFromForm(
          form.getPreferredBidderCountryId(), bindingResult).orElse(null);
    }

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        awardedContractModelAndView(scapId, activityId, bidParticipants, preselectedCountry),
        form,
        () -> {
          awardedContractService.saveAwardedContract(actualTenderActivity, form, bidParticipants);
          return ReverseRouter.redirect(on(ActualTenderSummaryController.class).renderActualTenderSummary(scapId));
        }
    );
  }

  private ModelAndView awardedContractModelAndView(ScapId scapId, Integer activityId,
                                                   List<InvitationToTenderParticipant> bidParticipants,
                                                   Map<String, String> preselectedCountry) {
    return new ModelAndView("scap/scap/actualtender/actualContractAward")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(BidParticipantsController.class)
                .renderBidParticipantsForm(scapId, activityId, null)))
        .addObject("bidParticipantsMap", AwardedContractFormService.getSelectOptions(bidParticipants))
        .addObject("countrySearchRestUrl",
            ReverseRouter.route(on(AwardedContractRestController.class).getCountrySearchResults(null)))
        .addObject("preselectedCountry", preselectedCountry)
        .addObject("paymentTermsRadioOptions", PaymentTermsRadio.getRadioOptions());
  }
}
