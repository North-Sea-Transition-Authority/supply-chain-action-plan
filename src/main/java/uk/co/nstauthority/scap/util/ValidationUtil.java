package uk.co.nstauthority.scap.util;

import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class ValidationUtil {

  //CAUTION: This does not change any freemarker template character limits.
  public static final int TEXT_AREA_STANDARD_LIMIT = 4000;

  private ValidationUtil() {
    throw new IllegalUtilClassInstantiationException(ValidationUtil.class);
  }
}
