package uk.co.nstauthority.scap.util;

import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.error.exception.IllegalUtilClassInstantiationException;

public class DateUtil {
  private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DateUtils.SHORT_DATE);

  private DateUtil() {
    throw new IllegalUtilClassInstantiationException(DateUtil.class);
  }

  public static String instantToString(Instant timeStamp) {
    return formatter.withZone(ZoneId.systemDefault()).format(timeStamp);
  }
}
