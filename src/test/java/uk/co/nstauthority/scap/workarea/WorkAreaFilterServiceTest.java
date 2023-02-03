package uk.co.nstauthority.scap.workarea;

import static org.assertj.core.api.Assertions.assertThat;
import static org.jooq.impl.DSL.upper;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAPS;
import static uk.co.nstauthority.scap.generated.jooq.Tables.SCAP_DETAILS;

import java.util.Collections;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@ExtendWith(MockitoExtension.class)
class WorkAreaFilterServiceTest {

  @InjectMocks
  WorkAreaFilterService workAreaFilterService;

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
}