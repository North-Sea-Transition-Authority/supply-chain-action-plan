package uk.co.nstauthority.scap.tasklist;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.util.ValidationUtil;

@ExtendWith(MockitoExtension.class)
class ValidationUtilTest {

  @Test
  void validationUtil_assertStatic() {
    assertThat(ValidationUtil.TEXT_AREA_STANDARD_LIMIT).isEqualTo(4000);
  }
}
