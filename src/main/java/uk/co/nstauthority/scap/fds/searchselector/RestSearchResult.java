package uk.co.nstauthority.scap.fds.searchselector;

import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.List;

/**
 * A RestSearchResult is used as response for producing options within a search selector.
 */
public class RestSearchResult {
  List<RestSearchItem> results;

  public RestSearchResult(@JsonProperty("results") List<RestSearchItem> results) {
    this.results = results;
  }

  public List<RestSearchItem> getResults() {
    return results;
  }
}