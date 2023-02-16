package uk.co.nstauthority.scap.notify;

import java.util.Set;
import javax.transaction.Transactional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.notify.library.FiviumNotificationClientApi;
import uk.co.fivium.notify.library.model.NotifyCallback;
import uk.co.fivium.notify.library.model.NotifyCallback.NotifyCallbackStatus;
import uk.co.fivium.notify.library.service.FiviumNotifyCallbackService;
import uk.co.nstauthority.scap.configuration.EmailConfiguration;
import uk.co.nstauthority.scap.notify.emailproperties.EmailDeliveryFailedEmailProps;
import uk.gov.service.notify.NotificationClientException;

@Service
public class NotifyCallbackService {
  private static final Logger LOGGER = LoggerFactory.getLogger(NotifyCallbackService.class);

  private static final Set<NotifyCallbackStatus> FAILURE_STATUSES = Set.of(
      NotifyCallbackStatus.PERMANENT_FAILURE,
      NotifyCallbackStatus.TEMPORARY_FAILURE
  );

  private final FiviumNotifyCallbackService fiviumNotifyCallbackService;
  private final NotifyEmailService notifyEmailService;
  private final FiviumNotificationClientApi fiviumNotificationClientApi;
  private final EmailConfiguration emailConfig;

  @Autowired
  public NotifyCallbackService(FiviumNotifyCallbackService fiviumNotifyCallbackService,
                               NotifyEmailService notifyEmailService,
                               FiviumNotificationClientApi fiviumNotificationClientApi,
                               EmailConfiguration emailConfig) {
    this.fiviumNotifyCallbackService = fiviumNotifyCallbackService;
    this.notifyEmailService = notifyEmailService;
    this.fiviumNotificationClientApi = fiviumNotificationClientApi;
    this.emailConfig = emailConfig;

    this.fiviumNotifyCallbackService.registerCallbackObserver(this::handleNotifyCallback);
  }

  @Transactional
  void handleNotifyCallback(NotifyCallback notifyCallback) {
    if (!FAILURE_STATUSES.contains(notifyCallback.getStatus())) {
      return;
    }

    LOGGER.info("The Notify provider could not deliver the message to the email address {}.", notifyCallback.getTo());

    var callbackEmail = emailConfig.callbackEmail();
    // if the failed email was going to the callback email address then return early
    if (notifyCallback.getTo().equals(callbackEmail)) {
      return;
    }

    try {
      var failedEmail = fiviumNotificationClientApi.getNotificationById(notifyCallback.getId());

      var failedEmailProperties = new EmailDeliveryFailedEmailProps(
          failedEmail.getEmailAddress()
              .orElseThrow(() -> new NotificationClientException("Failed email address cannot be retrieved")),
          failedEmail.getSubject().orElse(""),
          failedEmail.getBody()
      );

      notifyEmailService.sendEmail(failedEmailProperties, callbackEmail);

    } catch (NotificationClientException e) {
      LOGGER.error("Failing email properties cannot be retrieved", e);
    }
  }
}
