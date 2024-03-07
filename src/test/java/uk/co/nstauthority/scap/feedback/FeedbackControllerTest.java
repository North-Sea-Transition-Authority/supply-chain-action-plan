package uk.co.nstauthority.scap.feedback;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.flash;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.redirectedUrl;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = FeedbackController.class)
class FeedbackControllerTest extends AbstractControllerTest {


  @MockBean
  private FeedbackFormValidator feedbackFormValidator;

  @MockBean
  private FeedbackService feedbackService;

  private static final ScapId SCAP_ID = ScapId.valueOf(1111);

  @Test
  void renderFeedbackForm() throws Exception {
    mockMvc.perform(get(ReverseRouter.route(on(FeedbackController.class).renderFeedbackForm(SCAP_ID, null)))
            .with(authenticatedScapUser()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/feedback/giveFeedback"))
        .andExpect(model().attribute("satisfactionRadioItems", SatisfactionRating.getRadioItems()))
        .andExpect(model().attribute("scapId", SCAP_ID));
  }

  @Test
  void submitFeedbackForm_NoErrors_AssertSaveAndRedirect() throws Exception {
    var form = new FeedbackForm();

    when(feedbackFormValidator.validate(eq(form), any(BindingResult.class)))
        .thenReturn(ValidatorTestingUtil.bindingResultWithoutErrors(form));

    mockMvc.perform(post(ReverseRouter.route(on(FeedbackController.class).renderFeedbackForm(SCAP_ID, null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(redirectedUrl(ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null))))
        .andExpect(flash().attributeExists("notificationBannerView"));

    verify(feedbackService).saveFeedback(SCAP_ID, form, TestUserProvider.getUser());
  }

  @Test
  void submitFeedbackForm_WithErrors_AssertDoesNotSave() throws Exception {
    var form = new FeedbackForm();

    when(feedbackFormValidator.validate(eq(form), any(BindingResult.class)))
        .thenReturn(ValidatorTestingUtil.bindingResultWithErrors(form));

    mockMvc.perform(post(ReverseRouter.route(on(FeedbackController.class).renderFeedbackForm(SCAP_ID, null)))
            .with(authenticatedScapUser())
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/feedback/giveFeedback"))
        .andExpect(model().attribute("satisfactionRadioItems", SatisfactionRating.getRadioItems()))
        .andExpect(model().attribute("scapId", SCAP_ID))
        .andExpect(model().attributeExists("errorList"));

    verifyNoInteractions(feedbackService);
  }
}
