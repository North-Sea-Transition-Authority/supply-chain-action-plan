package uk.co.nstauthority.scap.mvc;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import javax.servlet.http.HttpServletRequest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.propertyeditors.StringTrimmerEditor;
import org.springframework.ui.Model;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import uk.co.nstauthority.scap.authentication.UserDetailService;
import uk.co.nstauthority.scap.branding.ServiceBrandingConfigurationProperties;
import uk.co.nstauthority.scap.error.exception.InvalidAuthenticationException;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationService;
import uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfiguration;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ControllerAdvice
class DefaultPageControllerAdvice {

  private static final Logger LOGGER = LoggerFactory.getLogger(DefaultPageControllerAdvice.class);

  private final ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties;
  private final TechnicalSupportConfiguration technicalSupportConfiguration;
  private final TopNavigationService topNavigationService;
  private final UserDetailService userDetailService;

  @Autowired
  DefaultPageControllerAdvice(ServiceBrandingConfigurationProperties serviceBrandingConfigurationProperties,
                              TechnicalSupportConfiguration technicalSupportConfiguration,
                              TopNavigationService topNavigationService,
                              UserDetailService userDetailService) {
    this.serviceBrandingConfigurationProperties = serviceBrandingConfigurationProperties;
    this.technicalSupportConfiguration = technicalSupportConfiguration;
    this.topNavigationService = topNavigationService;
    this.userDetailService = userDetailService;
  }

  @ModelAttribute
  void addDefaultModelAttributes(Model model, HttpServletRequest request) {
    addUser(model);
    addTopNavigationItems(model, request);
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
    model.addAttribute("serviceHomeUrl", ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null)));
  }

  private void addTechnicalSupportContactInfo(Model model) {
    model.addAttribute("technicalSupport", technicalSupportConfiguration.getTechnicalSupportConfigurationProperties());
  }

  private void addUser(Model model) {
    try {
      model.addAttribute("loggedInUser", userDetailService.getUserDetail());
    } catch (InvalidAuthenticationException e) {
      LOGGER.error("Could not get logged in user", e);
    }
  }

  private void addTopNavigationItems(Model model, HttpServletRequest request) {
    model.addAttribute("navigationItems", topNavigationService.getTopNavigationItems());
    model.addAttribute("currentEndPoint", request.getRequestURI());
  }
}
