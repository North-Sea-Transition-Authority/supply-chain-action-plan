package uk.co.nstauthority.scap.scap.util;


import static org.assertj.core.api.AssertionsForClassTypes.assertThat;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.util.DateUtil;

@ExtendWith(MockitoExtension.class)
public class DateUtilTest {

  private static final long EpochSecond = 946684800;


  @Test
  void dateUtil_InstantToString() {
    var instant = Instant.ofEpochSecond(EpochSecond);
    assertThat(DateUtil.instantToString(instant)).isEqualTo("1 Jan 2000");
  }

  @Test
  void dateUtil_LocalDateTimeToString() {
    var instant = LocalDate.ofInstant(Instant.ofEpochSecond(EpochSecond), ZoneId.systemDefault());
    assertThat(DateUtil.localDateToString(instant)).isEqualTo("1 Jan 2000");
  }
}
