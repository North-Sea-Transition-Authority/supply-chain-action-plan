package uk.co.nstauthority.scap.scap.casemanagement.furtherinfo;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.controllerhelper.ControllerHelperService;
import uk.co.nstauthority.scap.endpointvalidation.annotations.ScapHasStatus;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.RolePermission;
import uk.co.nstauthority.scap.permissionmanagement.endpointsecurity.PermissionsRequired;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventAction;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventService;
import uk.co.nstauthority.scap.scap.casemanagement.CaseEventSubject;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryController;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryModelAndViewGenerator;
import uk.co.nstauthority.scap.scap.summary.ScapSummaryViewService;

@Controller
@RequestMapping("{scapId}/")
@PermissionsRequired(permissions = RolePermission.REVIEW_SCAP)
@ScapHasStatus(permittedStatuses = ScapDetailStatus.SUBMITTED)
public class FurtherInfoController {

  private final CaseEventService caseEventService;

  private final ScapDetailService scapDetailService;

  private final ControllerHelperService controllerHelperService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final OrganisationGroupService organisationGroupService;

  private final FurtherInfoRequestFormValidator furtherInfoRequestFormValidator;

  @Autowired
  public FurtherInfoController(CaseEventService caseEventService,
                               ScapDetailService scapDetailService,
                               ControllerHelperService controllerHelperService,
                               ScapSummaryViewService scapSummaryViewService,
                               OrganisationGroupService organisationGroupService,
                               FurtherInfoRequestFormValidator furtherInfoRequestFormValidator) {
    this.caseEventService = caseEventService;
    this.scapDetailService = scapDetailService;
    this.controllerHelperService = controllerHelperService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.organisationGroupService = organisationGroupService;
    this.furtherInfoRequestFormValidator = furtherInfoRequestFormValidator;
  }

  @PostMapping(params = CaseEventAction.INFO_REQUESTED)
  public ModelAndView saveInfoRequestedForm(@PathVariable("scapId") ScapId scapId,
                                            @RequestParam(CaseEventAction.INFO_REQUESTED) String caseEventAction,
                                            @RequestParam("Info-Request-Panel") Boolean slideoutPanelOpen,
                                            @ModelAttribute("infoRequestForm") FurtherInfoRequestForm infoRequestForm,
                                            BindingResult bindingResult) {
    furtherInfoRequestFormValidator.validate(infoRequestForm, bindingResult);

    var scapDetail = scapDetailService.getLatestScapDetailByScapIdOrThrow(scapId);
    var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);
    var orgGroup = organisationGroupService
        .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(),
            "Get Org Group for Summary of SCAP ID: %s".formatted(scapId.scapId()));

    var generator =
        ScapSummaryModelAndViewGenerator.generator(scapDetail, scapSummary)
            .withCaseEventTimeline(caseEventService.getEventViewByScapId(scapId))
            .withFurtherInfoRequestFrom(infoRequestForm);
    orgGroup.ifPresent(generator::withOrgGroup);

    return controllerHelperService.checkErrorsAndRedirect(bindingResult,
        generator.generate(),
        infoRequestForm,
        () -> {
          caseEventService.recordNewEvent(CaseEventSubject.FURTHER_INFO_REQUESTED,
              scapId,
              scapDetail.getVersionNumber(),
              infoRequestForm.getInfoRequest().getInputValue());
          return ReverseRouter.redirect(on(ScapSummaryController.class).getScapSummary(scapId));
        });
  }
}
