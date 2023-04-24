package uk.co.nstauthority.scap.feedback;


import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("/feedback")
public class FeedbackController {

  private final FeedbackFormValidator feedbackFormValidator;
  private final ControllerHelperService controllerHelperService;
  private final FeedbackService feedbackService;
  private final UserDetailService userDetailService;

  @Autowired
  public FeedbackController(FeedbackFormValidator feedbackFormValidator,
                            ControllerHelperService controllerHelperService,
                            FeedbackService feedbackService,
                            UserDetailService userDetailService) {
    this.feedbackFormValidator = feedbackFormValidator;
    this.controllerHelperService = controllerHelperService;
    this.feedbackService = feedbackService;
    this.userDetailService = userDetailService;
  }

  @GetMapping(value = {"/{scapId}", "/"})
  public ModelAndView renderFeedbackForm(@PathVariable(value = "scapId", required = false) ScapId scapId,
                                         @ModelAttribute("form") FeedbackForm form) {
    return feedbackFormModelAndView(scapId);
  }

  @PostMapping(value = {"/{scapId}", "/"})
  ModelAndView submitFeedbackForm(@PathVariable(value = "scapId", required = false) ScapId scapId,
                                  @ModelAttribute("form") FeedbackForm form,
                                  BindingResult bindingResult) {
    bindingResult = feedbackFormValidator.validate(form, bindingResult);

    return controllerHelperService.checkErrorsAndRedirect(
        bindingResult,
        feedbackFormModelAndView(scapId),
        form,
        () -> {
          feedbackService.saveFeedback(scapId, form, userDetailService.getUserDetail());
          return ReverseRouter.redirect(on(WorkAreaController.class).getWorkArea(null));
        }
    );
  }

  private ModelAndView feedbackFormModelAndView(ScapId scapId) {
    return new ModelAndView("scap/feedback/giveFeedback")
        .addObject("satisfactionRadioItems", SatisfactionRating.getRadioItems())
        .addObject("scapId", scapId);
  }
}
