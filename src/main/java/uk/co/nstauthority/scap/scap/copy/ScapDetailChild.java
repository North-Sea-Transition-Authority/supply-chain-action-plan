package uk.co.nstauthority.scap.scap.copy;

import uk.co.nstauthority.scap.scap.detail.ScapDetail;

public interface ScapDetailChild {

  Integer getId();

  void setId(Integer id);

  ScapDetail getScapDetail();

  void setScapDetail(ScapDetail scapDetail);
}
