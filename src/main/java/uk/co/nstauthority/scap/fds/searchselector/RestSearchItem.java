package uk.co.nstauthority.scap.fds.searchselector;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * A RestSearchItem is used to produce each item within a search selector.
 * id and text are required fields for the JSON response.
 */
public record RestSearchItem(@JsonProperty("id") String id,
                             @JsonProperty("text") String text) {
}
