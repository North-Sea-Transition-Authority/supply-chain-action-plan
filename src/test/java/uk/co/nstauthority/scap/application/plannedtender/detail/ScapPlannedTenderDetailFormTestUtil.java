package uk.co.nstauthority.scap.application.plannedtender.detail;

class ScapPlannedTenderDetailFormTestUtil {

  static ScapPlannedTenderDetailForm getValidPlannedTenderDetailForm() {
    var form = new ScapPlannedTenderDetailForm();
    form.setAwardRationale("Test award rationale");
    form.setEstimatedValue("22.35");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setScopeDescription("Test scope description");
    return form;
  }
}
