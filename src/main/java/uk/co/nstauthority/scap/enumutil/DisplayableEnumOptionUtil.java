package uk.co.nstauthority.scap.enumutil;

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class DisplayableEnumOptionUtil {

  private DisplayableEnumOptionUtil() {
    throw new IllegalStateException("DisplayableEnumOptionUtil is a util class and should not be instantiated");
  }

  public static Map<String, String> getDisplayableOptions(
      Class<? extends Displayable> displayableOptionEnum
  ) {
    return Arrays.stream((Displayable[]) displayableOptionEnum.getEnumConstants())
        .sorted(Comparator.comparingInt(Displayable::getDisplayOrder))
        .collect(Collectors.toMap(
            Displayable::getEnumName,
            Displayable::getDisplayName,
            (x, y) -> y,
            LinkedHashMap::new
        ));
  }
}
