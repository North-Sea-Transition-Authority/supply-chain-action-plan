package uk.co.nstauthority.scap.utils;

import static org.assertj.core.api.Assertions.assertThat;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;
import org.apache.commons.lang3.reflect.FieldUtils;

public class ObjectTestingUtil {
  public static <T> void assertValuesEqual(T object1,
                                           T object2,
                                           Collection<String> ignoredForEqualsComparison) {

    // check that all fields apart from those we know will be different (or expect to be null) are equal between the two objects
    Arrays.stream(FieldUtils.getAllFields(object1.getClass()))
        .filter(field -> !ignoredForEqualsComparison.contains(field.getName()))
        .filter(field -> !field.isSynthetic())
        .forEach(field -> {

          var oldValue = getFieldValue(field, object1);
          var newValue = getFieldValue(field, object2);

          try {
            assertThat(oldValue).isEqualTo(newValue);
          } catch (AssertionError e) {
            throw new AssertionError(String.format("Expected [%s] field [%s] to be equal, object1 value [%s] != object2 value [%s]",
                object1.getClass(),
                field.getName(),
                oldValue,
                newValue));
          }
        });
  }

  private static Object getFieldValue(Field field, Object object) {
    try {
      return FieldUtils.readField(field, object, true);
    } catch (IllegalAccessException e) {
      throw new RuntimeException(
          String.format("Failed to access field '%s' on class '%s'", field.getName(), object.getClass()));
    }
  }
}
