package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.jooq.impl.DSL.exists;
import static org.jooq.impl.DSL.select;
import static org.jooq.impl.DSL.upper;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_DETAILS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_DETAIL_TYPES;
import static uk.co.nstauthority.scap.generated.jooq.Tables.PROJECT_FIELDS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAPS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_UPDATE_REQUESTS;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Collections;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;
import uk.co.nstauthority.scap.scap.projectdetails.ProjectType;

@ExtendWith(MockitoExtension.class)
class WorkAreaFilterServiceTest {

  private Clock clock =  Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  WorkAreaFilterService workAreaFilterService = new WorkAreaFilterService(clock);

  private WorkAreaFilter filter;
  private WorkAreaForm form;

  @BeforeEach
  void setup() {
    filter = new WorkAreaFilter();
    form = new WorkAreaForm();
  }

  @Test
  void getConditions_EmptyFilter_AssertEmpty() {
    var conditions = workAreaFilterService.getConditions(filter);
    assertThat(conditions).isEmpty();
  }

  @Test
  void getConditions_ScapStatusesSelected_AssertCondition() {
    var status = ScapDetailStatus.DRAFT;
    form.setScapStatuses(Collections.singletonList(ScapDetailStatus.DRAFT));
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);

    assertThat(conditions).containsExactly(
        SCAP_DETAILS.STATUS.in(Collections.singletonList(status.getEnumName()))
    );
  }

  @Test
  void getConditions_ScapReferenceEntered_AssertCondition() {
    var searchTerm = "1";
    form.setReferenceSearchTerm(searchTerm);
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);

    assertThat(conditions).containsExactly(
        upper(SCAPS.REFERENCE).contains(upper(searchTerm))
    );
  }

  @Test
  void getConditions_OrganisationSelected_AssertCondition() {
    var organisationId = 2;
    form.setOperatorId(organisationId);
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);

    assertThat(conditions).containsExactly(
        SCAPS.ORGANISATION_GROUP_ID.eq(organisationId)
    );
  }

  @Test
  void getConditions_FieldSelected_AssertCondition() {
    var fieldId = 2;
    form.setFieldId(fieldId);
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);

    assertThat(conditions).containsExactly(
        exists(
            select(PROJECT_FIELDS.FIELD_ID)
                .from(PROJECT_FIELDS)
                .where(PROJECT_FIELDS.FIELD_ID.eq(fieldId))
                .and(PROJECT_DETAILS.ID.eq(PROJECT_FIELDS.PROJECT_DETAILS_ID))
        )
    );
  }

  @Test
  void getConditions_ProjectTypeSelected_AssertCondition() {
    var projectType = ProjectType.CARBON_STORAGE_PERMIT;
    var projectTypes = Collections.singletonList(projectType);
    var projectTypeStrings = Collections.singletonList(projectType.getEnumName());

    form.setProjectTypes(projectTypes);
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);

    assertThat(conditions).containsExactly(
        exists(
            select(PROJECT_DETAIL_TYPES.ID)
                .from(PROJECT_DETAIL_TYPES)
                .where(PROJECT_DETAIL_TYPES.PROJECT_TYPE.in(projectTypeStrings))
                .and(PROJECT_DETAILS.ID.eq(PROJECT_DETAIL_TYPES.PROJECT_DETAIL_ID))
        )
    );
  }

  @Test
  void getConditions_UpdateRequestStatus_NullScapConditions() {
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);
    assertThat(conditions).isEmpty();
  }

  @Test
  void getConditions_UpdateRequestStatus_AllScapConditions() {
    form.setUpdateRequestStatusRadioOptions(UpdateRequestStatusRadioOptions.ALL);
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);
    assertThat(conditions).isEmpty();
  }

  @Test
  void getConditions_UpdateRequestStatus_AnyUpdatesConditions() {
    form.setUpdateRequestStatusRadioOptions(UpdateRequestStatusRadioOptions.UPDATE_REQUESTED);
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);
    assertThat(conditions)
        .containsExactly(SCAP_UPDATE_REQUESTS.DUE_DATE.isNotNull(), SCAP_UPDATE_REQUESTS.RESOLUTION_DATE.isNull());
  }

  @Test
  void getConditions_UpdateRequestStatus_OverdueUpdatesConditions() {
    form.setUpdateRequestStatusRadioOptions(UpdateRequestStatusRadioOptions.UPDATE_OVERDUE);
    filter.update(form);

    var conditions = workAreaFilterService.getConditions(filter);
    assertThat(conditions).containsExactly(
        SCAP_UPDATE_REQUESTS.DUE_DATE.le(LocalDateTime.now(clock)),
        SCAP_UPDATE_REQUESTS.RESOLUTION_DATE.isNull());
  }
}