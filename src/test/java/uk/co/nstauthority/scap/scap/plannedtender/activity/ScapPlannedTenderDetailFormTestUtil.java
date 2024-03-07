package uk.co.nstauthority.scap.scap.plannedtender.activity;

import uk.co.nstauthority.scap.scap.RemunerationModel;

class ScapPlannedTenderDetailFormTestUtil {

  static PlannedTenderActivityForm getValidPlannedTenderDetailForm() {
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale("Test award rationale");
    form.setEstimatedValue("22.35");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setScopeDescription("Test scope description");
    return form;
  }
}
