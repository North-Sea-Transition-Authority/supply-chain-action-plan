package uk.co.nstauthority.scap.scap.projectperformance;

import java.time.LocalDate;
import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;

public class ProjectPerformanceForm {

  private Boolean projectCompleted;

  private final ThreeFieldDateInput startDate;
  private final ThreeFieldDateInput completionDate;
  private final DecimalInput outturnCost;

  public ProjectPerformanceForm() {
    this.startDate = new ThreeFieldDateInput("startDate", "actual execution start date");
    this.completionDate = new ThreeFieldDateInput("completionDate", "actual commissioning or completion date");
    this.outturnCost = new DecimalInput("outturnCost", "the project outturn cost");
  }

  public Boolean getProjectCompleted() {
    return projectCompleted;
  }

  public void setProjectCompleted(Boolean projectCompleted) {
    this.projectCompleted = projectCompleted;
  }

  public ThreeFieldDateInput getStartDate() {
    return startDate;
  }

  public void setStartDate(LocalDate startDate) {
    this.startDate.setDate(startDate);
  }

  public ThreeFieldDateInput getCompletionDate() {
    return completionDate;
  }

  public void setCompletionDate(LocalDate completionDate) {
    this.completionDate.setDate(completionDate);
  }

  public DecimalInput getOutturnCost() {
    return outturnCost;
  }

  public void setOutturnCost(String outturnCost) {
    this.outturnCost.setInputValue(outturnCost);
  }
}
