package uk.co.nstauthority.scap.scap.copy;

import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;

public interface PlannedTenderChild {

  void setId(Integer id);

  void setPlannedTender(PlannedTender plannedTender);

  Integer getId();

  PlannedTender getPlannedTender();
}