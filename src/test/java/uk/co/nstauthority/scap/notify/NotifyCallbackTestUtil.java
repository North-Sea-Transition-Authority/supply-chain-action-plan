package uk.co.nstauthority.scap.notify;

import java.time.Instant;
import uk.co.fivium.notify.library.model.NotifyCallback;

class NotifyCallbackTestUtil {

  static final String CALLBACK_TOKEN = "test-token!1";
  static final String BOUNCE_BACK_EMAIL_BOX = "bounceback@gov.com";

  static NotifyCallback createNotifyCallback(String to, NotifyCallback.NotifyCallbackStatus notifyCallbackStatus) {
    return new NotifyCallback(
        "be0a4c7d-1657-4b83-8771-2a40e7408d67",
        345235,
        notifyCallbackStatus,
        to,
        NotifyCallback.NotifyNotificationType.EMAIL,
        Instant.now(),
        Instant.now(),
        Instant.now()
    );
  }
}
