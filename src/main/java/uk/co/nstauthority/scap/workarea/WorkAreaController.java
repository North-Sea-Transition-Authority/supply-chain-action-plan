package uk.co.nstauthority.scap.workarea;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.application.start.ScapStartController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Controller
@RequestMapping("/work-area")
public class WorkAreaController {

  @GetMapping
  public ModelAndView getWorkArea() {
    return new ModelAndView("scap/workarea/workArea")
        .addObject("startScapUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap()));
  }
}
