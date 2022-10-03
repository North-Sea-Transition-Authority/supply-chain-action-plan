package uk.co.nstauthority.scap.fds.searchselector;

import java.util.List;

public class RestSearchResult {

  private final List<RestSearchSingleResult> results;

  public RestSearchResult(List<RestSearchSingleResult> results) {
    this.results = results;
  }

  public List<RestSearchSingleResult> getResults() {
    return results;
  }

}
