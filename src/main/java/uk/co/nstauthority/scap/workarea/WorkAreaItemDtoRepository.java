package uk.co.nstauthority.scap.workarea;

import static org.jooq.impl.DSL.field;
import static uk.co.nstauthority.scap.generated.jooq.Tables.ACTUAL_TENDERS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.CONTRACTING_PERFORMANCE_OVERVIEWS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PLANNED_TENDERS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_DETAILS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_PERFORMANCES;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAPS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.jooq.Condition;
import org.jooq.DSLContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@Repository
class WorkAreaItemDtoRepository {

  private final DSLContext context;

  @Autowired
  WorkAreaItemDtoRepository(DSLContext context) {
    this.context = context;
  }

  // Using not in to get all results not in the given statuses
  List<WorkAreaItemDto> getAllByScapStatusNotIn(ScapDetailStatus... statuses) {
    var statusStrings = Arrays.stream(statuses).map(ScapDetailStatus::getEnumName).toList();
    var condition = SCAP_DETAILS.STATUS.notIn(statusStrings);
    return performQuery(Collections.singletonList(condition));
  }

  // Using in to get the results for the given organisation groups
  List<WorkAreaItemDto> getAllByOrganisationGroups(List<Integer> organisationGroupIds) {
    var condition = SCAPS.ORGANISATION_GROUP_ID.in(organisationGroupIds);
    return performQuery(Collections.singletonList(condition));
  }

  private List<WorkAreaItemDto> performQuery(List<Condition> conditions) {
    return context.select(
            SCAPS.SCAP_ID,
            SCAP_DETAILS.VERSION_NUMBER,
            SCAPS.REFERENCE,
            PROJECT_DETAILS.PROJECT_NAME,
            SCAPS.ORGANISATION_GROUP_ID,
            SCAP_DETAILS.STATUS,
            PROJECT_PERFORMANCES.IS_PROJECT_COMPLETED,
            CONTRACTING_PERFORMANCE_OVERVIEWS.HAS_CONTRACTING_PERFORMANCE,
            ACTUAL_TENDERS.HAS_ACTUAL_TENDERS,
            PLANNED_TENDERS.HAS_PLANNED_TENDERS,
            SCAP_DETAILS.CREATED_TIMESTAMP,
            SCAP_DETAILS.SUBMITTED_TIMESTAMP
        )
        .from(SCAPS)
        .join(SCAP_DETAILS).on((SCAP_DETAILS.SCAP_ID).eq(SCAPS.SCAP_ID))
        .leftJoin(PROJECT_DETAILS).on(field(PROJECT_DETAILS.SCAP_DETAIL_ID).eq(SCAP_DETAILS.ID))
        .leftJoin(PROJECT_PERFORMANCES).on(field(PROJECT_PERFORMANCES.SCAP_DETAIL_ID).eq(SCAP_DETAILS.ID))
        .leftJoin(CONTRACTING_PERFORMANCE_OVERVIEWS).on(CONTRACTING_PERFORMANCE_OVERVIEWS.SCAP_DETAIL_ID.eq(SCAP_DETAILS.ID))
        .leftJoin(ACTUAL_TENDERS).on(ACTUAL_TENDERS.SCAP_DETAIL_ID.eq(SCAP_DETAILS.ID))
        .leftJoin(PLANNED_TENDERS).on(PLANNED_TENDERS.SCAP_DETAIL_ID.eq(SCAP_DETAILS.ID))
        .where(conditions)
        .and(field(SCAP_DETAILS.TIP_FLAG).eq(true))
        .fetchInto(WorkAreaItemDto.class);
  }
}
