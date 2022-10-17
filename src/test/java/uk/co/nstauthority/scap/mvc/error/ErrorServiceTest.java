package uk.co.nstauthority.scap.mvc.error;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.web.servlet.ModelAndView;
import uk.co.nstauthority.scap.error.ErrorConfiguration;
import uk.co.nstauthority.scap.error.ErrorConfigurationProperties;
import uk.co.nstauthority.scap.error.ErrorService;

@ExtendWith(SpringExtension.class)
@EnableConfigurationProperties(value = ErrorConfigurationProperties.class)
@ContextConfiguration(classes = ErrorConfiguration.class)
@TestPropertySource("classpath:application-test.properties")
class ErrorServiceTest {

  private static final String ERROR_REF_ATTRIBUTE_NAME = "errorRef";
  private static final String SERVICE_HOME_URLS_ATTRIBUTE_NAME = "serviceHomeUrl";
  private static final String STACK_TRACE_ATTRIBUTE = "stackTrace";
  private static final String UNSAFE_CHARACTERS = "0125AEIOULNSZ";

  @Autowired
  private ErrorConfiguration errorConfiguration;

  private ErrorService errorService;

  @BeforeEach
  public void setup() {
    errorService = new ErrorService(errorConfiguration);
  }

  @Test
  void addErrorAttributesToModel_whenThrowableError_assertExpectedModelAttributes() {
    final var resultingModelMap =  errorService.addErrorAttributesToModel(
        new ModelAndView(),
        new NullPointerException()
    ).getModelMap();

    assertThat(resultingModelMap).containsOnlyKeys(
        ERROR_REF_ATTRIBUTE_NAME,
        SERVICE_HOME_URLS_ATTRIBUTE_NAME,
        STACK_TRACE_ATTRIBUTE
    );
    Object errorRef = resultingModelMap.get(ERROR_REF_ATTRIBUTE_NAME);
    assertThat(errorRef).isNotNull();
    assertThat((String) errorRef).doesNotContainPattern(UNSAFE_CHARACTERS);
    assertThat(resultingModelMap.get(STACK_TRACE_ATTRIBUTE)).isNotNull();
  }

  @Test
  void addErrorAttributesToModel_whenNoThrowableError_assertExpectedModelAttributes() {
    final var resultingModelMap =  errorService.addErrorAttributesToModel(
        new ModelAndView(),
        null
    ).getModelMap();

    assertThat(resultingModelMap).containsOnlyKeys(
        SERVICE_HOME_URLS_ATTRIBUTE_NAME
    );
  }
}
