package uk.co.nstauthority.scap.endpointvalidation.rules;

import static org.assertj.core.api.Assertions.assertThat;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.platform.commons.util.AnnotationUtils;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;

@ExtendWith(MockitoExtension.class)
abstract class AbstractInterceptorRuleTest {

  protected static final ScapId SCAP_ID = new ScapId(1000);

  @Mock
  protected HttpServletRequest request;

  @Mock
  protected HttpServletResponse response;

  @Mock
  protected Scap scap;

  @Mock
  protected ScapDetail scapDetail;

  protected ServiceUserDetail userDetail = TestUserProvider.getUser();

  protected <A extends Annotation> A getAnnotation(Class<?> controllerClass, Class<A> annotationClass) {
    var annotation = AnnotationUtils.findAnnotation(controllerClass, annotationClass);
    assertThat(annotation).isPresent();
    return annotation.get();
  }

  protected <A extends Annotation> A getAnnotation(Method method, Class<A> annotationClass) {
    var annotation = AnnotationUtils.findAnnotation(method, annotationClass);
    assertThat(annotation).isPresent();
    return annotation.get();
  }

}
