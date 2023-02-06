package uk.co.nstauthority.scap.workarea;

import static org.jooq.impl.DSL.exists;
import static org.jooq.impl.DSL.select;
import static org.jooq.impl.DSL.upper;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_DETAILS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_DETAIL_TYPES;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_FIELDS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAPS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.jooq.Condition;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;

@Service
class WorkAreaFilterService {

  ArrayList<Condition> getConditions(WorkAreaFilter filter) {
    var conditions = new ArrayList<Condition>();

    if (Objects.nonNull(filter.getScapStatuses())) {
      conditions.add(getStatusCondition(filter.getScapStatuses()));
    }

    if (StringUtils.isNotBlank(filter.getReferenceSearchTerm())) {
      conditions.add(getReferenceCondition(filter.getReferenceSearchTerm()));
    }

    if (Objects.nonNull(filter.getOperatorId())) {
      conditions.add(getOperatorCondition(filter.getOperatorId()));
    }

    if (Objects.nonNull(filter.getFieldId())) {
      conditions.add(getFieldCondition(filter.getFieldId()));
    }

    if (Objects.nonNull(filter.getProjectTypes())) {
      conditions.add(getProjectTypesCondition(filter.getProjectTypes()));
    }

    return conditions;
  }

  private Condition getProjectTypesCondition(List<ProjectType> projectTypes) {
    var projectTypeStrings = projectTypes.stream()
        .map(ProjectType::getEnumName)
        .toList();

    return exists(
        select(PROJECT_DETAIL_TYPES.ID)
            .from(PROJECT_DETAIL_TYPES)
            .where(PROJECT_DETAIL_TYPES.PROJECT_TYPE.in(projectTypeStrings))
            .and(PROJECT_DETAILS.ID.eq(PROJECT_DETAIL_TYPES.PROJECT_DETAIL_ID))
    );
  }

  private Condition getFieldCondition(Integer fieldId) {
    return exists(
        select(PROJECT_FIELDS.FIELD_ID)
        .from(PROJECT_FIELDS)
        .where(PROJECT_FIELDS.FIELD_ID.eq(fieldId))
        .and(PROJECT_DETAILS.ID.eq(PROJECT_FIELDS.PROJECT_DETAILS_ID))
    );
  }

  private Condition getOperatorCondition(Integer operatorId) {
    return SCAPS.ORGANISATION_GROUP_ID.eq(operatorId);
  }

  private Condition getReferenceCondition(String referenceSearchTerm) {
    return upper(SCAPS.REFERENCE).contains(upper(referenceSearchTerm));
  }

  private Condition getStatusCondition(List<ScapDetailStatus> statuses) {
    var statusStrings = statuses
        .stream()
        .map(ScapDetailStatus::getEnumName)
        .toList();
    return SCAP_DETAILS.STATUS.in(statusStrings);
  }
}
