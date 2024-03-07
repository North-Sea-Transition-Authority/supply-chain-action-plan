package uk.co.nstauthority.scap.utils;

import java.time.Instant;
import java.util.Calendar;

public class EntityTestingUtil {

  public static Instant dateToInstant(Integer year, Integer month, Integer day) {
    var calendar = Calendar.getInstance();
    calendar.set(year, month - 1, day);
    var date = calendar.getTime();
    return date.toInstant();
  }
}
