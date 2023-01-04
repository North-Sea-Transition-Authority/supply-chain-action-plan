package uk.co.nstauthority.scap.scap.summary;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectDetailsService;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("{scapId}")
public class ScapSummaryController {

  private final ScapDetailService scapDetailService;

  private final ScapSummaryViewService scapSummaryViewService;

  private final ProjectDetailsService projectDetailsService;

  private final OrganisationGroupService organisationGroupService;

  public ScapSummaryController(ScapDetailService scapDetailService, ScapSummaryViewService scapSummaryViewService,
                               ProjectDetailsService projectDetailsService,
                               OrganisationGroupService organisationGroupService) {
    this.scapDetailService = scapDetailService;
    this.scapSummaryViewService = scapSummaryViewService;
    this.projectDetailsService = projectDetailsService;
    this.organisationGroupService = organisationGroupService;
  }

  @GetMapping
  public ModelAndView getScapSummary(@PathVariable("scapId") Integer scapId) {
    var scapDetailOptional = scapDetailService.getLatestScapDetailByScapId(scapId);
    if (scapDetailOptional.isPresent()) {
      var scapDetail = scapDetailOptional.get();
      var scapSummary = scapSummaryViewService.getScapSummaryView(scapDetail);

      var orgGroup = organisationGroupService
          .getOrganisationGroupById(scapDetail.getScap().getOrganisationGroupId(), "Get Org Group for Summary");

      return new ModelAndView("scap/scap/summary/scapSummaryOverview")
          .addObject("scapSummaryView", scapSummary)
          .addObject("projectReference", scapDetail.getScap().getReference())
          .addObject("projectName", scapSummary.projectDetailsSummaryView().projectName())
          .addObject("operator", orgGroup.map(OrganisationGroup::getName).orElse(""))
          .addObject("scapStatus", scapDetail.getStatus())
          .addObject("scapSubmissionStatus",
              scapSummaryViewService.inferSubmissionStatusFromSummary(scapSummary).getDisplayName())
          .addObject("backLinkUrl", ReverseRouter.route(on(WorkAreaController.class)
              .getWorkArea()));
    }
    throw new ScapEntityNotFoundException("Could not find details for SCAP of ID: %s".formatted(scapId));
  }
}
