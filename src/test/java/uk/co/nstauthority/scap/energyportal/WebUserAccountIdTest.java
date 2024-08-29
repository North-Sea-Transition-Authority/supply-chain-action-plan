package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.springframework.web.server.ResponseStatusException;

class WebUserAccountIdTest {

  @Test
  void valueOf_validWebUserAccountId() {
    assertThat(WebUserAccountId.valueOf("10"))
        .extracting(WebUserAccountId::id)
        .isEqualTo(10L);
  }

  @Test
  void valueOf_nonNumericWebUserAccountId() {
    var nonNumericValue = "non numeric string";
    assertThatThrownBy(() -> WebUserAccountId.valueOf(nonNumericValue))
        .isInstanceOf(ResponseStatusException.class)
        .hasMessageContaining("Cannot find WebUserAccount with ID: %s".formatted(nonNumericValue));
  }
}