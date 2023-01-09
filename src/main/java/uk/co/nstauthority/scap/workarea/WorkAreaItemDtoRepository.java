package uk.co.nstauthority.scap.workarea;

import static org.jooq.impl.DSL.field;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.jooq.Condition;
import org.jooq.DSLContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import uk.co.nstauthority.scap.jooq.JooqService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@Repository
class WorkAreaItemDtoRepository {

  private final DSLContext context;
  private final JooqService jooqService;

  @Autowired
  WorkAreaItemDtoRepository(DSLContext context, JooqService jooqService) {
    this.context = context;
    this.jooqService = jooqService;
  }

  // Using trueCondition to get all results
  // TODO: SCAP2022-69, replace with filters for only SCAPs the regulator wants to see
  List<WorkAreaItemDto> getAllByScapStatusNotIn(ScapDetailStatus... statuses) {
    var statusStrings = Arrays.stream(statuses).map(ScapDetailStatus::getEnumName).toList();
    var condition = field("sd.status").notIn(statusStrings);
    return performQuery(Collections.singletonList(condition));
  }

  // Using in to get the results for the given organisation groups
  List<WorkAreaItemDto> getAllByOrganisationGroups(List<Integer> organisationGroupIds) {
    var condition = field("s.organisation_group_id").in(organisationGroupIds);
    return performQuery(Collections.singletonList(condition));
  }

  private List<WorkAreaItemDto> performQuery(List<Condition> conditions) {
    return context.select(
            field("s.scap_id"),
            field("sd.version_number"),
            field("s.reference"),
            field("pd.project_name"),
            field("s.organisation_group_id"),
            field("sd.status"),
            field("pp.is_project_completed"),
            field("cpo.has_contracting_performance"),
            field("at.has_actual_tenders"),
            field("pt.has_planned_tenders"),
            field("sd.created_timestamp"),
            field("sd.submitted_timestamp")
        )
        .from(jooqService.scapTable("scaps").as("s"))
        .join(jooqService.scapTable("scap_details").as("sd")).on(field("sd.scap_id").eq(field("s.scap_id")))
        .leftJoin(jooqService.scapTable("project_details").as("pd")).on(field("pd.scap_detail_id").eq(field("sd.id")))
        .leftJoin(jooqService.scapTable("project_performances").as("pp")).on(field("pp.scap_detail_id").eq(field("sd.id")))
        .leftJoin(jooqService.scapTable("contracting_performance_overviews").as("cpo"))
        .on(field("cpo.scap_detail_id").eq(field("sd.id")))
        .leftJoin(jooqService.scapTable("actual_tenders").as("at")).on(field("at.scap_detail_id").eq(field("sd.id")))
        .leftJoin(jooqService.scapTable("planned_tenders").as("pt")).on(field("pt.scap_detail_id").eq(field("sd.id")))
        .where(conditions)
        .and(field("sd.tip_flag").eq(true))
        .fetchInto(WorkAreaItemDto.class);
  }
}
