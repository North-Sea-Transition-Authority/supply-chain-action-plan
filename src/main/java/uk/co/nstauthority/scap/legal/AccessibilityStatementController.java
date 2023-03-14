package uk.co.nstauthority.scap.legal;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
@RequestMapping("/accessibility-statement")
public class AccessibilityStatementController {

  @GetMapping
  public ModelAndView renderAccessibilityStatement() {
    return new ModelAndView("scap/legal/accessibilityStatement");
  }
}
