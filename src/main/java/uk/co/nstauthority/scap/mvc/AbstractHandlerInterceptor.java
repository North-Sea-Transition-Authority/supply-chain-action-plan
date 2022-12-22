package uk.co.nstauthority.scap.mvc;

import java.lang.annotation.Annotation;
import java.lang.reflect.Parameter;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

public abstract class AbstractHandlerInterceptor implements HandlerInterceptor {

  public boolean hasAnnotation(HandlerMethod handlerMethod, Class<? extends Annotation> annotation) {
    return handlerMethod.hasMethodAnnotation(annotation)
        || handlerMethod.getMethod().getDeclaringClass().isAnnotationPresent(annotation);
  }

  public Annotation getAnnotation(HandlerMethod handlerMethod, Class<? extends Annotation> annotation) {
    return Objects.requireNonNullElse(
        handlerMethod.getMethodAnnotation(annotation),
        handlerMethod.getMethod().getDeclaringClass().getAnnotation(annotation)
    );
  }

  public static Optional<Parameter> getPathVariableByClass(HandlerMethod handlerMethod, Class<?> clazzOfPathVariable) {
    return Arrays.stream(handlerMethod.getMethod().getParameters())
        .filter(methodParameter -> methodParameter.getType().equals(clazzOfPathVariable)
            && methodParameter.isAnnotationPresent(PathVariable.class))
        .findFirst();
  }
}