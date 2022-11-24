package uk.co.nstauthority.scap.enumutil;

import java.util.Arrays;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;
import uk.co.nstauthority.scap.util.StreamUtils;

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

  public static Map<String, String> getDisplayableOptionsWithDescription(Class<? extends Displayable> displayableOptionEnum) {
    return Arrays.stream((DisplayableEnumOption[]) displayableOptionEnum.getEnumConstants())
        .sorted(Comparator.comparingInt(Displayable::getDisplayOrder))
        .collect(StreamUtils.toLinkedHashMap(
            DisplayableEnumOption::getEnumName,
            opt -> "%s (%s)".formatted(opt.getDescription(), opt.getDisplayName())
        ));
  }
}
