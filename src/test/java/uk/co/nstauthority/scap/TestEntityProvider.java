package uk.co.nstauthority.scap;

import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;

public class TestEntityProvider {
  public static Scap getScap() {
    return ScapEntityTestUtil.scapBuilder().build();
  }

  public static ScapDetail getScapDetail() {
    return ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScap(getScap())
        .build();
  }
}
