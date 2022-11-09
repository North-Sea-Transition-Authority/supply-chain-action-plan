package uk.co.nstauthority.scap.scap.start;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("/new")
public class ScapStartController {

  private final String workAreaUrl = ReverseRouter.route(on(WorkAreaController.class).getWorkArea());
  private final String startRedirectUrl = ReverseRouter.route(on(OrganisationGroupController.class)
      .renderNewScapOrganisationGroupForm(null));

  @GetMapping
  public ModelAndView renderStartNewScap() {
    return new ModelAndView("scap/application/start")
        .addObject("startScapRedirectUrl", startRedirectUrl)
        .addObject("backLinkUrl", workAreaUrl);
  }
}
