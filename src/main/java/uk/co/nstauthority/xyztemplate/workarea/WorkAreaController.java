package uk.co.nstauthority.xyztemplate.workarea;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
@RequestMapping("/work-area")
public class WorkAreaController {

  @GetMapping
  public ModelAndView getWorkArea() {
    return new ModelAndView("xyzt/workarea/workArea");
  }
}
