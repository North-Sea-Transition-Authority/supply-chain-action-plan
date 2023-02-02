package uk.co.nstauthority.scap.mvc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.branding.CustomerConfigurationProperties;
import uk.co.nstauthority.scap.branding.ServiceConfigurationProperties;
import uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@WebMvcTest
@ActiveProfiles("test")
@ContextConfiguration(classes = {
    DefaultPageControllerAdviceTest.TestController.class,
    DefaultPageControllerAdvice.class
})
@WithMockUser
class DefaultPageControllerAdviceTest extends AbstractControllerTest {

  @Test
  void addDefaultModelAttributes_verifyDefaultAttributes() throws Exception {

    var modelAndView = mockMvc.perform(
        get(ReverseRouter.route(on(TestController.class).testEndpoint()))
    )
        .andReturn()
        .getModelAndView();

    assertThat(modelAndView).isNotNull();

    var modelMap = modelAndView.getModel();

    assertThat(modelMap).containsOnlyKeys(
        "customerBranding",
        "org.springframework.validation.BindingResult.customerBranding",
        "serviceBranding",
        "org.springframework.validation.BindingResult.serviceBranding",
        "technicalSupport",
        "org.springframework.validation.BindingResult.technicalSupport",
        "serviceHomeUrl",
        "loggedInUser",
        "navigationItems",
        "currentEndPoint",
        "org.springframework.validation.BindingResult.loggedInUser"
    );

    assertThat((CustomerConfigurationProperties) modelMap.get("customerBranding")).hasNoNullFieldsOrProperties();
    assertThat((ServiceConfigurationProperties) modelMap.get("serviceBranding")).hasNoNullFieldsOrProperties();
    assertThat((TechnicalSupportConfigurationProperties) modelMap.get("technicalSupport")).hasNoNullFieldsOrProperties();
    assertThat(modelMap.get("serviceHomeUrl")).isEqualTo(
        ReverseRouter.route(on(WorkAreaController.class).getWorkArea(null))
    );
  }

  // Dummy application to stop the @WebMvcTest loading more than it needs
  @SpringBootApplication
  static class TestApplication {
  }

  @RequestMapping("/endpoint")
  static class TestController {

    @GetMapping()
    ModelAndView testEndpoint() {
      return new ModelAndView();
    }
  }

}