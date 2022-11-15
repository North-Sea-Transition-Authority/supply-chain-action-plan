package uk.co.nstauthority.scap.scap.actualtender.activity.bidparticipants;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.List;
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
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderControllerRedirectionService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityController;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipant;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Controller
@RequestMapping("{scapId}/actual-tender/activity/{activityId}/bid-participants")
public class BidParticipantsController {

  private final ScapService scapService;
  private final ActualTenderActivityService actualTenderActivityService;
  private final BidParticipantsFormService bidParticipantsFormService;
  private final ControllerHelperService controllerHelperService;
  private final ActualTenderControllerRedirectionService actualTenderControllerRedirectionService;
  private final InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Autowired
  public BidParticipantsController(ScapService scapService,
                                   ActualTenderActivityService actualTenderActivityService,
                                   BidParticipantsFormService bidParticipantsFormService,
                                   ControllerHelperService controllerHelperService,
                                   ActualTenderControllerRedirectionService actualTenderControllerRedirectionService,
                                   InvitationToTenderParticipantService invitationToTenderParticipantService) {
    this.scapService = scapService;
    this.actualTenderActivityService = actualTenderActivityService;
    this.bidParticipantsFormService = bidParticipantsFormService;
    this.controllerHelperService = controllerHelperService;
    this.actualTenderControllerRedirectionService = actualTenderControllerRedirectionService;
    this.invitationToTenderParticipantService = invitationToTenderParticipantService;
  }

  @GetMapping
  public ModelAndView renderBidParticipantsForm(@PathVariable("scapId") Integer scapId,
                                                @PathVariable("activityId") Integer activityId,
                                                @ModelAttribute("form") BidParticipantsForm form) {
    scapService.getScapById(scapId);
    var actualTenderDetail = actualTenderActivityService.getById(activityId);
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderDetail);
    var existingBidParticipants = InvitationToTenderParticipantService
        .getBidParticipantsFromInvitationToTenderParticipants(invitationToTenderParticipants);
    form.setSelectedBidParticipantIds(BidParticipantsFormService.getParticipantIds(existingBidParticipants));

    return bidParticipantsFormModelAndView(scapId, invitationToTenderParticipants, activityId);
  }

  @PostMapping
  public ModelAndView saveBidParticipantsForm(@PathVariable("scapId") Integer scapId,
                                              @PathVariable("activityId") Integer activityId,
                                              @ModelAttribute("form") BidParticipantsForm form,
                                              BindingResult bindingResult) {
    scapService.getScapById(scapId);
    var actualTenderDetail = actualTenderActivityService.getById(activityId);
    var invitationToTenderParticipants = invitationToTenderParticipantService
        .getInvitationToTenderParticipants(actualTenderDetail);

    bindingResult = bidParticipantsFormService.validate(form, bindingResult, invitationToTenderParticipants);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        bidParticipantsFormModelAndView(scapId, invitationToTenderParticipants, activityId),
        form,
        () -> {
          invitationToTenderParticipantService
              .updateBidParticipants(invitationToTenderParticipants, form.getSelectedBidParticipantIds());
          return actualTenderControllerRedirectionService.redirectFromBidParticipantsForm(scapId, actualTenderDetail);
        }
    );
  }

  private ModelAndView bidParticipantsFormModelAndView(Integer scapId, List<InvitationToTenderParticipant> participants,
                                                       Integer activityId) {
    return new ModelAndView("scap/scap/actualtender/bidParticipants")
        .addObject("backLinkUrl", ReverseRouter.route(on(ActualTenderActivityController.class)
            .renderExistingActualTenderActivityForm(scapId, activityId)))
        .addObject("bidParticipantCheckboxes",
            BidParticipantsFormService.getBidParticipantsCheckboxes(participants));
  }
}
