package uk.co.nstauthority.scap.application.plannedtender.list;

import uk.co.nstauthority.scap.application.plannedtender.detail.ScapPlannedTenderDetail;

public record PlannedTenderDetailListItem(ScapPlannedTenderDetail detail, String changeLinkUrl, String deleteLinkUrl) {

}
