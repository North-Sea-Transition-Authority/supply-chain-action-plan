package uk.co.nstauthority.scap.scap.projectperformance;

import uk.co.fivium.formlibrary.input.DecimalInput;
import uk.co.fivium.formlibrary.input.IntegerInput;
import uk.co.fivium.formlibrary.input.ThreeFieldDateInput;
import uk.co.nstauthority.scap.enumutil.YesNo;

public class ProjectPerformanceForm {

  private YesNo isProjectCompleted;
  private final IntegerInput startDay;
  private final IntegerInput startMonth;
  private final IntegerInput startYear;
  private final IntegerInput completionDay;
  private final IntegerInput completionMonth;
  private final IntegerInput completionYear;
  private final ThreeFieldDateInput startDate;
  private final ThreeFieldDateInput completionDate;
  private final DecimalInput outturnCost;

  public ProjectPerformanceForm() {
    this.startDay = new IntegerInput("startDay", "start day");
    this.startMonth = new IntegerInput("startMonth", "start month");
    this.startYear = new IntegerInput("startYear", "start year");
    this.completionDay = new IntegerInput("completionDay", "completion day");
    this.completionMonth = new IntegerInput("completionMonth", "completion month");
    this.completionYear = new IntegerInput("completionYear", "completion year");
    this.startDate = new ThreeFieldDateInput(
        "startDate",
        "actual execution start date",
        this.startDay,
        this.startMonth,
        this.startYear
    );
    this.completionDate = new ThreeFieldDateInput(
        "completionDate",
        "actual commissioning or completion date",
        this.completionDay,
        this.completionMonth,
        this.completionYear
    );
    this.outturnCost = new DecimalInput("outturnCost", "the project outturn cost");
  }

  public YesNo getIsProjectCompleted() {
    return isProjectCompleted;
  }

  public void setIsProjectCompleted(YesNo isProjectCompleted) {
    this.isProjectCompleted = isProjectCompleted;
  }

  public ThreeFieldDateInput getStartDate() {
    return startDate;
  }

  public ThreeFieldDateInput getCompletionDate() {
    return completionDate;
  }

  public IntegerInput getStartDay() {
    return startDay;
  }

  public IntegerInput getStartMonth() {
    return startMonth;
  }

  public IntegerInput getStartYear() {
    return startYear;
  }

  public IntegerInput getCompletionDay() {
    return completionDay;
  }

  public IntegerInput getCompletionMonth() {
    return completionMonth;
  }

  public IntegerInput getCompletionYear() {
    return completionYear;
  }

  public void setStartDay(String startDay) {
    this.startDay.setInputValue(startDay);
  }

  public void setStartMonth(String startMonth) {
    this.startMonth.setInputValue(startMonth);
  }

  public void setStartYear(String startYear) {
    this.startYear.setInputValue(startYear);
  }

  public void setCompletionDay(String completionDay) {
    this.completionDay.setInputValue(completionDay);
  }

  public void setCompletionMonth(String completionMonth) {
    this.completionMonth.setInputValue(completionMonth);
  }

  public void setCompletionYear(String completionYear) {
    this.completionYear.setInputValue(completionYear);
  }

  public DecimalInput getOutturnCost() {
    return outturnCost;
  }

  public void setOutturnCost(String outturnCost) {
    this.outturnCost.setInputValue(outturnCost);
  }
}
