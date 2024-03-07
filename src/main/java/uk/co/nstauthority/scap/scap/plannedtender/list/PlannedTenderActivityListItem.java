package uk.co.nstauthority.scap.scap.plannedtender.list;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Objects;
import uk.co.fivium.formlibrary.validator.date.DateUtils;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.plannedtender.activity.PlannedTenderActivity;
import uk.co.nstauthority.scap.scap.plannedtender.activity.delete.DeletePlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.plannedtender.activity.update.UpdatePlannedTenderActivityController;
import uk.co.nstauthority.scap.scap.scap.ScapId;

public record PlannedTenderActivityListItem(PlannedTenderActivity detail,
                                            String indicativeActualTenderStartDate,
                                            String indicativeContractAwardDate,
                                            String changeLinkUrl,
                                            String deleteLinkUrl) {
  public static PlannedTenderActivityListItem from(ScapId scapId, PlannedTenderActivity plannedTenderActivity) {
    return new PlannedTenderActivityListItem(
        plannedTenderActivity,
        DateUtils.format(plannedTenderActivity.getExpectedActualTenderStartDate()),
        DateUtils.format(plannedTenderActivity.getExpectedContractAwardDate()),
        ReverseRouter.route(on(UpdatePlannedTenderActivityController.class)
            .renderUpdatePlannedTenderDetail(scapId, plannedTenderActivity.getId())),
        ReverseRouter.route(on(DeletePlannedTenderActivityController.class)
            .deletePlannedTenderDetail(scapId, plannedTenderActivity.getId(), null))
    );
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PlannedTenderActivityListItem that = (PlannedTenderActivityListItem) o;
    return Objects.equals(detail, that.detail)
        && Objects.equals(indicativeActualTenderStartDate, that.indicativeActualTenderStartDate)
        && Objects.equals(indicativeContractAwardDate, that.indicativeContractAwardDate)
        && Objects.equals(changeLinkUrl, that.changeLinkUrl)
        && Objects.equals(deleteLinkUrl, that.deleteLinkUrl);
  }
}
