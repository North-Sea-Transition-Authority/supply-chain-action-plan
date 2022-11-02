package uk.co.nstauthority.scap.application.plannedtender.detail;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.application.detail.ScapDetailService;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderController;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderService;
import uk.co.nstauthority.scap.application.plannedtender.hasplannedtender.ScapHasPlannedTenderController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@Controller
@RequestMapping("{scapId}/planned-tender/activity")
public class ScapPlannedTenderDetailController {

  private final ScapOverviewService scapOverviewService;
  private final ScapDetailService scapDetailService;
  private final ScapPlannedTenderService scapPlannedTenderService;
  private final ScapPlannedTenderDetailService scapPlannedTenderDetailService;
  private final ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService;
  private final ValidationErrorOrderingService validationErrorOrderingService;

  @Autowired
  public ScapPlannedTenderDetailController(ScapOverviewService scapOverviewService, ScapDetailService scapDetailService,
                                           ScapPlannedTenderService scapPlannedTenderService,
                                           ScapPlannedTenderDetailService scapPlannedTenderDetailService,
                                           ScapPlannedTenderDetailFormService scapPlannedTenderDetailFormService,
                                           ValidationErrorOrderingService validationErrorOrderingService) {
    this.scapOverviewService = scapOverviewService;
    this.scapDetailService = scapDetailService;
    this.scapPlannedTenderService = scapPlannedTenderService;
    this.scapPlannedTenderDetailService = scapPlannedTenderDetailService;
    this.scapPlannedTenderDetailFormService = scapPlannedTenderDetailFormService;
    this.validationErrorOrderingService = validationErrorOrderingService;
  }

  @GetMapping
  public ModelAndView renderPlannedTenderDetailForm(@PathVariable("scapId") Integer scapId,
                                                    @ModelAttribute("form") ScapPlannedTenderDetailForm form) {
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var scapPlannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);

    return plannedTenderDetailFormModelAndView(scapId, getBackLinkUrl(scapId, scapPlannedTender));

  }

  @PostMapping
  public ModelAndView savePlannedTenderDetailForm(@PathVariable("scapId") Integer scapId,
                                                  @ModelAttribute("form") ScapPlannedTenderDetailForm form,
                                                  BindingResult bindingResult) {
    bindingResult = scapPlannedTenderDetailFormService.validate(bindingResult, form);
    var scap = scapOverviewService.getScapById(scapId);
    var scapDetail = scapDetailService.getLatestScapDetailByScapOrThrow(scap);
    var scapPlannedTender = scapPlannedTenderService.getScapPlannedTenderByScapDetailOrThrow(scapDetail);
    if (bindingResult.hasErrors()) {
      return plannedTenderDetailFormModelAndView(scapId, getBackLinkUrl(scapId, scapPlannedTender))
          .addObject("errorItems", validationErrorOrderingService.getErrorItemsFromBindingResult(form, bindingResult));
    }

    scapPlannedTenderDetailService.createPlannedTenderDetail(scapPlannedTender, form);

    return ReverseRouter.redirect(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId));
  }

  private String getBackLinkUrl(Integer scapId, ScapPlannedTender scapPlannedTender) {
    if (scapPlannedTenderDetailService.hasExistingTenderDetails(scapPlannedTender)) {
      return ReverseRouter.route(on(ScapPlannedTenderController.class).renderPlannedTenderActivities(scapId));
    }
    return ReverseRouter.route(on(ScapHasPlannedTenderController.class).renderHasPlannedTenderActivityForm(scapId));
  }

  private ModelAndView plannedTenderDetailFormModelAndView(Integer scapId, String backLinkUrl) {
    return new ModelAndView("scap/application/plannedtender/plannedTenderActivityDetail")
        .addObject("backLinkUrl", backLinkUrl)
        .addObject("submitPostUrl",
            ReverseRouter.route(on(ScapPlannedTenderDetailController.class)
                .savePlannedTenderDetailForm(scapId, null, emptyBindingResult())))
        .addObject("remunerationModels", RemunerationModel.getRemunerationModels());
  }
}
