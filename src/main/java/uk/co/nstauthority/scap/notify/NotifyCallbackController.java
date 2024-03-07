package uk.co.nstauthority.scap.notify;

import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;
import uk.co.fivium.notify.library.model.NotifyCallback;
import uk.co.fivium.notify.library.service.FiviumNotifyCallbackAccessDeniedException;
import uk.co.fivium.notify.library.service.FiviumNotifyCallbackService;
import uk.co.nstauthority.scap.error.exception.NotifyCallbackAccessDeniedException;

/**
 * Rest controller to handle GOV.UK Notify callback requests. A callback message is formatted in JSON.
 * For more information about callbacks visit the
 * <a href="https://docs.notifications.service.gov.uk/java.html#callbacks">GovUK Notifications Docs</a>
 */
@RestController
public class NotifyCallbackController {

  private final FiviumNotifyCallbackService fiviumNotifyCallbackService;

  public NotifyCallbackController(FiviumNotifyCallbackService fiviumNotifyCallbackService) {
    this.fiviumNotifyCallbackService = fiviumNotifyCallbackService;
  }

  @PostMapping("/notify/callback")
  public ResponseEntity<Object> notifyCallback(@RequestBody NotifyCallback callbackRequest,
                                               @RequestHeader("Authorization") String bearerToken) {
    try {
      fiviumNotifyCallbackService.handleCallback(callbackRequest, bearerToken);
    } catch (FiviumNotifyCallbackAccessDeniedException e) {
      throw new NotifyCallbackAccessDeniedException(e);
    }
    return ResponseEntity.ok().build();
  }
}
