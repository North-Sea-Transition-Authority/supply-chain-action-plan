package uk.co.nstauthority.scap.legal;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.branding.BusinessSupportConfiguration;

@Controller
@RequestMapping("/contact")
public class ContactInformationController {

  private final BusinessSupportConfiguration businessSupportConfiguration;

  @Autowired
  public ContactInformationController(BusinessSupportConfiguration businessSupportConfiguration) {
    this.businessSupportConfiguration = businessSupportConfiguration;
  }

  @GetMapping
  public ModelAndView renderContactInformation() {
    return new ModelAndView("scap/legal/contactInformation")
        .addObject("businessSupport", businessSupportConfiguration);
  }
}
