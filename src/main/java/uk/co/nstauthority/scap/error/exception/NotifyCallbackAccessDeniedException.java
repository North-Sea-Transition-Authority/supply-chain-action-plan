package uk.co.nstauthority.scap.error.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;
import uk.co.fivium.notify.library.service.FiviumNotifyCallbackAccessDeniedException;

@ResponseStatus(value = HttpStatus.FORBIDDEN, reason = "Access request for NotifyCallback cannot be authorised")
public class NotifyCallbackAccessDeniedException extends RuntimeException {
  public NotifyCallbackAccessDeniedException(FiviumNotifyCallbackAccessDeniedException exception) {
    super(exception.getMessage(), exception);
  }
}
