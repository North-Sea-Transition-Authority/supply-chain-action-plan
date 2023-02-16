package uk.co.nstauthority.scap.notify;


import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.notify.library.FiviumNotificationClientApi;
import uk.co.fivium.notify.library.model.NotifyCallback;
import uk.co.fivium.notify.library.service.FiviumNotifyCallbackService;
import uk.co.nstauthority.scap.configuration.EmailConfiguration;
import uk.co.nstauthority.scap.notify.emailproperties.EmailDeliveryFailedEmailProps;
import uk.gov.service.notify.Notification;
import uk.gov.service.notify.NotificationClientException;

@ExtendWith(MockitoExtension.class)
class NotifyCallbackServiceTest {

  @Mock
  private FiviumNotifyCallbackService fiviumNotifyCallbackService;

  @Mock
  private NotifyEmailService notifyEmailService;

  @Mock
  private FiviumNotificationClientApi fiviumNotificationClientApi;
  private NotifyCallbackService notifyCallbackService;
  private final EmailConfiguration emailConfig = new EmailConfiguration("test", "", NotifyCallbackTestUtil.BOUNCE_BACK_EMAIL_BOX);

  @BeforeEach
  void setUp() {
    notifyCallbackService = new NotifyCallbackService(fiviumNotifyCallbackService, notifyEmailService,
        fiviumNotificationClientApi, emailConfig);
  }

  @ParameterizedTest
  @EnumSource(value = NotifyCallback.NotifyCallbackStatus.class, names = {"PERMANENT_FAILURE", "TEMPORARY_FAILURE"}, mode = EnumSource.Mode.EXCLUDE)
  void handleNotifyCallback_WrongNotifyCallbackStatus_DoesNotSendEmailToCallbackEmail(
      NotifyCallback.NotifyCallbackStatus notifyCallbackStatus) {
    var notifyCallback = NotifyCallbackTestUtil.createNotifyCallback("test@email.co.uk", notifyCallbackStatus);

    notifyCallbackService.handleNotifyCallback(notifyCallback);

    verify(notifyEmailService, never()).sendEmail(any(), any());
  }

  @Test
  void handleNotifyCallback_EmailToCallbackEmailFailed_DoesNotResendEmailToCallbackEmail() {
    var notifyCallback = NotifyCallbackTestUtil.createNotifyCallback(NotifyCallbackTestUtil.BOUNCE_BACK_EMAIL_BOX,
        NotifyCallback.NotifyCallbackStatus.TEMPORARY_FAILURE);

    notifyCallbackService.handleNotifyCallback(notifyCallback);

    verify(notifyEmailService, never()).sendEmail(any(), any());
  }

  @Test
  void handleNotifyCallback_SendsNotificationOfFailedEmailToCallbackEmail() throws NotificationClientException {
    var emailAddress = "test@email.co.uk";
    var subject = "subject";
    var body = "body";
    var notifyCallback = NotifyCallbackTestUtil.createNotifyCallback(emailAddress,
        NotifyCallback.NotifyCallbackStatus.TEMPORARY_FAILURE);

    var notification = Mockito.mock(Notification.class);
    when(notification.getEmailAddress()).thenReturn(Optional.of(emailAddress));
    when(notification.getSubject()).thenReturn(Optional.of(subject));
    when(notification.getBody()).thenReturn(body);
    when(fiviumNotificationClientApi.getNotificationById(notifyCallback.getId())).thenReturn(notification);

    var argumentCaptor = ArgumentCaptor.forClass(EmailDeliveryFailedEmailProps.class);
    notifyCallbackService.handleNotifyCallback(notifyCallback);

    verify(notifyEmailService).sendEmail(argumentCaptor.capture(), eq(NotifyCallbackTestUtil.BOUNCE_BACK_EMAIL_BOX));
    assertThat(argumentCaptor.getValue())
        .extracting(
            EmailDeliveryFailedEmailProps::getFailedEmailAddress,
            EmailDeliveryFailedEmailProps::getOriginalSubject,
            EmailDeliveryFailedEmailProps::getOriginalBody
        )
        .containsExactly(
            emailAddress,
            subject,
            body
        );
  }
}
