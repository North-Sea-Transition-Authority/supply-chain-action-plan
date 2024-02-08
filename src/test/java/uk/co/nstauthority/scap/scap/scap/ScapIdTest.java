package uk.co.nstauthority.scap.scap.scap;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.springframework.web.server.ResponseStatusException;

class ScapIdTest {

  @Test
  void valueOf_validScapId() {
    assertThat(ScapId.valueOf("10"))
        .extracting(ScapId::scapId)
        .isEqualTo(10);
  }

  @Test
  void valueOf_nonNumericScapId() {
    var nonNumericValue = "non numeric string";
    assertThatThrownBy(() -> ScapId.valueOf(nonNumericValue))
        .isInstanceOf(ResponseStatusException.class)
        .hasMessageContaining("Cannot find Scap with ID: %s".formatted(nonNumericValue));
  }
}