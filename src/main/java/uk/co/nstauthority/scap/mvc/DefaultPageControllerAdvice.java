package uk.co.nstauthority.scap.mvc;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import javax.servlet.http.HttpServletRequest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.propertyeditors.StringTrimmerEditor;
import org.springframework.ui.Model;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.ServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfiguration;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ControllerAdvice
class DefaultPageControllerAdvice {

  private final ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties;
  private final TechnicalSupportConfiguration technicalSupportConfiguration;
  private final UserDetailService userDetailService;

  @Autowired
  DefaultPageControllerAdvice(ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties,
                              TechnicalSupportConfiguration technicalSupportConfiguration,
                              UserDetailService userDetailService) {
    this.serviceBrandingConfigurationProperties = serviceBrandingConfigurationProperties;
    this.technicalSupportConfiguration = technicalSupportConfiguration;
    this.userDetailService = userDetailService;
  }

  @ModelAttribute
  void addDefaultModelAttributes(Model model, HttpServletRequest request) {
    addUser(model);
    addBrandingAttributes(model);
    addCommonUrls(model);
    addTechnicalSupportContactInfo(model);
  }

  @InitBinder
  void initBinder(WebDataBinder binder) {
    // Trim whitespace from form fields
    binder.registerCustomEditor(String.class, new StringTrimmerEditor(true));
  }

  private void addBrandingAttributes(Model model) {
    model.addAttribute("serviceBranding", serviceBrandingConfigurationProperties.getServiceConfigurationProperties());
    model.addAttribute("customerBranding", serviceBrandingConfigurationProperties.getCustomerConfigurationProperties());
  }

  private void addCommonUrls(Model model) {
    model.addAttribute("serviceHomeUrl", ReverseRouter.route(on(WorkAreaController.class).getWorkArea()));
  }

  private void addTechnicalSupportContactInfo(Model model) {
    model.addAttribute("technicalSupport", technicalSupportConfiguration.getTechnicalSupportConfigurationProperties());
  }

  private void addUser(Model model) {
    model.addAttribute("loggedInUser", userDetailService.getUserDetail());
  }
}
