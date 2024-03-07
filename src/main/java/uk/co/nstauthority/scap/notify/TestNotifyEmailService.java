package uk.co.nstauthority.scap.notify;

import java.util.Arrays;
import javax.transaction.Transactional;
import org.apache.commons.validator.routines.EmailValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;
import uk.co.fivium.notify.library.FiviumNotificationClientApi;
import uk.co.nstauthority.scap.branding.ServiceConfigurationProperties;
import uk.co.nstauthority.scap.configuration.EmailConfiguration;
import uk.gov.service.notify.NotificationClientException;

@Service
@ConditionalOnProperty(name = "email.mode", havingValue = "test")
public class TestNotifyEmailService implements NotifyEmailService {
  private static final Logger LOGGER = LoggerFactory.getLogger(TestNotifyEmailService.class);

  private final FiviumNotificationClientApi fiviumNotificationClientApi;
  private final ServiceConfigurationProperties serviceConfigurationProperties;
  private final EmailValidator emailValidator;
  private final EmailConfiguration emailConfig;

  public TestNotifyEmailService(FiviumNotificationClientApi fiviumNotificationClientApi,
                                ServiceConfigurationProperties serviceConfigurationProperties,
                                EmailValidator emailValidator,
                                EmailConfiguration emailConfig) {
    this.fiviumNotificationClientApi = fiviumNotificationClientApi;
    this.serviceConfigurationProperties = serviceConfigurationProperties;
    this.emailValidator = emailValidator;
    this.emailConfig = emailConfig;
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
    var emailSentCounter = 0;

    // Set the TEST_EMAIL personalisation when in the development service
    var personalisation = emailProperties.getEmailPersonalisations();
    personalisation.put("TEST_EMAIL", "yes");
    personalisation.put("SERVICE_NAME", serviceConfigurationProperties.name());


    var recipients = Arrays.stream(emailConfig.testRecipientList().split(","))
        .map(String::trim)
        .toList();

    // If we have test recipients send the email to each
    for (var testRecipient : recipients) {
      if (!emailValidator.isValid(testRecipient)) {
        LOGGER.error("Recipient email is not a valid email address: {}", testRecipient);
        continue;
      }

      try {
        fiviumNotificationClientApi.sendEmail(
            emailProperties.getTemplate().getTemplateName(), testRecipient, personalisation, reference, emailReplyToId);

        emailSentCounter++;
      } catch (NotificationClientException e) {
        LOGGER.error("Error sending test email", e);
      }
    }

    // If we managed to send at least one email to test recipients then return true, else false
    return emailSentCounter > 0;
  }
}
