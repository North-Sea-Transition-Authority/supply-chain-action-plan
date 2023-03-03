package uk.co.nstauthority.scap.fds.searchselector;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Utilities intended for use with FDS Search Selector and Add to List components with manual entry.
 */
public class ManualEntryUtil {

  private ManualEntryUtil() {
    throw new IllegalStateException("ManualEntryUtil is a utility class and should not be instantiated");
  }

  public static final String FREE_TEXT_PREFIX = "FT_";
  public static final String HTML_SPACE_REPLACEMENT = "--space--";
  public static final String HTML_DOUBLE_QUOTE_REPLACEMENT = "--double-quote--";
  public static final String HTML_OPEN_BRACKET_REPLACEMENT = "--open-bracket--";
  public static final String HTML_CLOSE_BRACKET_REPLACEMENT = "--close-bracket--";

  /**
   * Adds a manual entry to a {@link RestSearchResult}, with ID of "{@value FREE_TEXT_PREFIX}manualEntryText" and text
   *     of "manualEntry".
   *
   * @param searchResult The original RestSearchResult obtained by API call
   * @param manualEntry The manual entry you want to add to the result list, usually the user search term
   * @return A new RestSearchResult with the input manual entry added
   */
  public static RestSearchResult addManualEntry(RestSearchResult searchResult, String manualEntry) {
    var manualEntryItem = new RestSearchItem(addFreeTextPrefix(manualEntry), manualEntry);

    var resultsWithManualEntry = new ArrayList<>(searchResult.getResults());
    resultsWithManualEntry.add(0, manualEntryItem);

    return new RestSearchResult(resultsWithManualEntry);
  }

  /**
   * Prefixes the entered string with {@value FREE_TEXT_PREFIX}.
   *
   * @param target The string to add the prefix to
   * @return The string with the prefix added
   */
  public static String addFreeTextPrefix(String target) {
    // TODO SCAP2022-244: When team-wide strategy for free text is decided, remove this
    target = target.replace(" ", HTML_SPACE_REPLACEMENT);
    target = target.replace("\"", HTML_DOUBLE_QUOTE_REPLACEMENT);
    target = target.replace("(", HTML_OPEN_BRACKET_REPLACEMENT);
    target = target.replace(")", HTML_CLOSE_BRACKET_REPLACEMENT);
    target = target.replaceAll("[^A-Za-z\\d-. ]", "");

    return "%s%s".formatted(FREE_TEXT_PREFIX, target);
  }

  public static String removeFreeTextPrefix(String target) {
    // TODO SCAP2022-244: When team-wide strategy for free text is decided, remove this
    target = target.replaceFirst(FREE_TEXT_PREFIX, "");
    target = target.replace(HTML_SPACE_REPLACEMENT, " ");
    target = target.replace(HTML_DOUBLE_QUOTE_REPLACEMENT, "\"");
    target = target.replace(HTML_OPEN_BRACKET_REPLACEMENT, "(");
    target = target.replace(HTML_CLOSE_BRACKET_REPLACEMENT, ")");
    return target;
  }

  /**
   * Splits a list of string IDs returned by an Add to List component with manual entry into lists of
   *     manual entries and actual IDs.
   *
   * @param idAndManualEntryList combined list of IDs and manual entries returned by add to list component
   * @return a ManualEntryPartitionedList, with a list of integer IDs, and a list of Strings with the FREE_TEXT_PREFIX
   *     removed
   * @throws NumberFormatException if the list of strings *not* prefixed with {@value FREE_TEXT_PREFIX} contains
   *     something that is not a number
   */
  public static ManualEntryPartitionedList partitionManualEntries(List<String> idAndManualEntryList) {
    var partitionedList =  idAndManualEntryList.stream()
        .filter(Objects::nonNull)
        .collect(Collectors.groupingBy(
            idOrManualEntry -> idOrManualEntry.startsWith(FREE_TEXT_PREFIX)
        ));
    var idList = partitionedList.getOrDefault(false, Collections.emptyList())
        .stream()
        .map(Integer::valueOf)
        .toList();
    var manualEntryList = partitionedList.getOrDefault(true, Collections.emptyList())
        .stream()
        .map(ManualEntryUtil::removeFreeTextPrefix)
        .toList();
    return new ManualEntryPartitionedList(idList, manualEntryList);
  }
}
