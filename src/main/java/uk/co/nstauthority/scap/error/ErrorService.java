package uk.co.nstauthority.scap.error;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.exception.ExceptionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Service
public class ErrorService {

  private final ErrorConfiguration errorConfiguration;
  private static final Logger LOGGER = LoggerFactory.getLogger(ErrorService.class);
  private static final String SAFE_CHARACTERS = "BCDFGHJKMPQRTVWXY346789";

  private final TopNavigationService topNavigationService;

  @Autowired
  public ErrorService(ErrorConfiguration errorConfiguration, TopNavigationService topNavigationService) {
    this.errorConfiguration = errorConfiguration;
    this.topNavigationService = topNavigationService;
  }

  private String generateErrorReference() {
    return RandomStringUtils.random(9, SAFE_CHARACTERS.toUpperCase());
  }

  public ModelAndView addErrorAttributesToModel(ModelAndView modelAndView, Throwable throwable) {
    if (throwable != null) {
      addStackTraceToModel(modelAndView, throwable);
      addErrorReference(modelAndView, throwable);
    }
    addCommonUrls(modelAndView);
    addBrandingConfigs(modelAndView);
    addTechnicalSupportConfigs(modelAndView);
    addTopNavigation(modelAndView);
    return modelAndView;
  }

  private void addCommonUrls(ModelAndView modelAndView) {
    modelAndView.addObject("serviceHomeUrl", ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null)));
  }

  private void addBrandingConfigs(ModelAndView modelAndView) {
    modelAndView.addObject("serviceBranding", errorConfiguration
        .getServiceBrandingConfigurationProperties()
        .getServiceConfigurationProperties());
    modelAndView.addObject("customerBranding", errorConfiguration
        .getServiceBrandingConfigurationProperties()
        .getCustomerConfigurationProperties());
  }

  private void addTechnicalSupportConfigs(ModelAndView modelAndView) {
    modelAndView.addObject("technicalSupport", errorConfiguration
        .getTechnicalSupportConfigurationProperties());
  }

  private boolean isStackTraceEnabled() {
    return errorConfiguration.getErrorConfigurationProperties().stackTraceEnabled();
  }

  private void addStackTraceToModel(ModelAndView modelAndView, Throwable throwable) {
    if (isStackTraceEnabled() && throwable != null) {
      modelAndView.addObject("stackTrace", ExceptionUtils.getStackTrace(throwable));
    }
  }

  private void addErrorReference(ModelAndView modelAndView, Throwable throwable) {
    if (throwable != null) {
      var errorReference = generateErrorReference();
      modelAndView.addObject("errorRef", errorReference);
      LOGGER.error("Caught unhandled exception (errorRef {})", errorReference, throwable);
    }
  }

  private void addTopNavigation(ModelAndView modelAndView) {
    modelAndView.addObject("navigationItems", topNavigationService.getTopNavigationItems());
    modelAndView.addObject("currentEndPoint", "#");
  }
}
