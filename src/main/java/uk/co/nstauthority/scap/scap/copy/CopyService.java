package uk.co.nstauthority.scap.scap.copy;

import uk.co.nstauthority.scap.scap.detail.NewScapType;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

public interface CopyService {

  int runOrder();

  void copyEntity(ScapDetail oldScapDetail, ScapDetail newScapDetail, NewScapType newScapType);
}
