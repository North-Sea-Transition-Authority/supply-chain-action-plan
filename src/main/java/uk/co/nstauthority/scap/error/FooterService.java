package uk.co.nstauthority.scap.error;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Map;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.legal.AccessibilityStatementController;
import uk.co.nstauthority.scap.legal.ContactInformationController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@Service
public class FooterService {

  private final CustomerConfigurationProperties customerConfigurationProperties;

  @Autowired
  public FooterService(CustomerConfigurationProperties customerConfigurationProperties) {
    this.customerConfigurationProperties = customerConfigurationProperties;
  }

  public void addFooterItems(Map<String, Object> model) {
    model.put("accessibilityStatementUrl",
        ReverseRouter.route(on(AccessibilityStatementController.class).renderAccessibilityStatement()));
    model.put("contactUrl",
        ReverseRouter.route(on(ContactInformationController.class).renderContactInformation()));
    model.put("privacyStatementUrl", customerConfigurationProperties.privacyStatementUrl());
  }
}
