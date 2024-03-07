package uk.co.nstauthority.scap.notify;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import uk.co.fivium.notify.library.model.NotifyCallback;
import uk.co.fivium.notify.library.service.FiviumNotifyCallbackAccessDeniedException;
import uk.co.fivium.notify.library.service.FiviumNotifyCallbackService;
import uk.co.nstauthority.scap.AbstractControllerTestWithSecurity;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ContextConfiguration(classes = {NotifyCallbackController.class})
class NotifyCallbackControllerTest extends AbstractControllerTestWithSecurity {

  @Autowired
  private ObjectMapper objectMapper;
  @MockBean
  FiviumNotifyCallbackService fiviumNotifyCallbackService;

  @Test
  void notifyCallback_assertStatusOk() throws Exception {
    var notifyCallback = NotifyCallbackTestUtil.createNotifyCallback("test@email.com", NotifyCallback.NotifyCallbackStatus.TEMPORARY_FAILURE);
    mockMvc.perform(
        post(ReverseRouter.route(on(NotifyCallbackController.class).notifyCallback(notifyCallback, NotifyCallbackTestUtil.CALLBACK_TOKEN)))
            .header("Authorization", NotifyCallbackTestUtil.CALLBACK_TOKEN)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(notifyCallback)))
        .andExpect(status().isOk())
    ;
  }

  @Test
  void notifyCallback_InvalidBearerToken_assertStatusForbidden() throws Exception {
    var notifyCallback = NotifyCallbackTestUtil.createNotifyCallback("test@email.com", NotifyCallback.NotifyCallbackStatus.TEMPORARY_FAILURE);

    doThrow(new FiviumNotifyCallbackAccessDeniedException(""))
        .when(fiviumNotifyCallbackService).handleCallback(any(), any());

    mockMvc.perform(
        post(ReverseRouter.route(on(NotifyCallbackController.class).notifyCallback(null, null)))
            .header("Authorization", NotifyCallbackTestUtil.CALLBACK_TOKEN)
            .contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(notifyCallback)))
        .andExpect(status().isForbidden());
  }
}