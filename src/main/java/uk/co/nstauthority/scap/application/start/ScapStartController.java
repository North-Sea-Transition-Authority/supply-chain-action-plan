package uk.co.nstauthority.scap.application.start;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Controller
@RequestMapping("/new")
public class ScapStartController {

  @GetMapping
  public ModelAndView renderStartNewScap() {
    return new ModelAndView("scap/application/start")
        // TODO SCAP2022-119: Replace with redirect to OrganisationGroup form
        .addObject("startScapRedirectUrl", "#")
        .addObject("backLinkUrl",
            ReverseRouter.route(on(WorkAreaController.class).getWorkArea()));
  }
}
