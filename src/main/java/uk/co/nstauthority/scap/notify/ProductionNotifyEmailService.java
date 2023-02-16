package uk.co.nstauthority.scap.notify;

import javax.transaction.Transactional;
import org.apache.commons.validator.routines.EmailValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;
import uk.co.fivium.notify.library.FiviumNotificationClientApi;
import uk.co.nstauthority.scap.branding.ServiceConfigurationProperties;
import uk.gov.service.notify.NotificationClientException;

@Service
@ConditionalOnProperty(name = "email.mode", havingValue = "production")
public class ProductionNotifyEmailService implements NotifyEmailService {
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductionNotifyEmailService.class);

  private final FiviumNotificationClientApi fiviumNotificationClientApi;
  private final ServiceConfigurationProperties serviceConfigurationProperties;
  private final EmailValidator emailValidator;

  public ProductionNotifyEmailService(FiviumNotificationClientApi fiviumNotificationClientApi,
                                      ServiceConfigurationProperties serviceConfigurationProperties,
                                      EmailValidator emailValidator) {
    this.fiviumNotificationClientApi = fiviumNotificationClientApi;
    this.serviceConfigurationProperties = serviceConfigurationProperties;
    this.emailValidator = emailValidator;
  }

  @Transactional
  @Override
  public boolean sendEmail(EmailProperties emailProperties, String toEmailAddress) {
    return sendEmail(emailProperties, toEmailAddress, null, null);
  }

  @Transactional
  @Override
  public boolean sendEmail(EmailProperties emailProperties,
                           String toEmailAddress,
                           String reference,
                           String emailReplyToId) {

    if (!emailValidator.isValid(toEmailAddress)) {
      return false;
    }

    var personalisation = emailProperties.getEmailPersonalisations();
    personalisation.put("SERVICE_NAME", serviceConfigurationProperties.name());

    try {
      fiviumNotificationClientApi
          .sendEmail(emailProperties.getTemplate().getTemplateName(), toEmailAddress, personalisation, reference, emailReplyToId);
      return true;
    } catch (NotificationClientException e) {
      LOGGER.error("Error constructing NotificationClient", e);
      return false;
    }
  }
}
