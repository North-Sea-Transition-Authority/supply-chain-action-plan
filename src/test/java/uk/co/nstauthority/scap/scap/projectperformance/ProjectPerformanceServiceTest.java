package uk.co.nstauthority.scap.scap.projectperformance;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;

@SuppressWarnings("OptionalGetWithoutIsPresent")
@ExtendWith(MockitoExtension.class)
class ProjectPerformanceServiceTest {

  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @Mock
  ProjectPerformanceRepository projectPerformanceRepository;

  ProjectPerformanceService projectPerformanceService;

  @BeforeEach
  void setup() {
    projectPerformanceService = new ProjectPerformanceService(projectPerformanceRepository, clock);
  }

  @Test
  void findByScapDetail() {
    var scapDetail = new ScapDetail();
    var projectPerformance = new ProjectPerformance(scapDetail, Instant.now());

    when(projectPerformanceRepository.findByScapDetail(scapDetail)).thenReturn(Optional.of(projectPerformance));

    var returnedProjectPerformance = projectPerformanceService.findByScapDetail(scapDetail);

    assertThat(returnedProjectPerformance).contains(projectPerformance);
  }

  @Test
  void saveProjectPerformance_Yes_VerifySaves() {
    var argumentCaptor = ArgumentCaptor.forClass(ProjectPerformance.class);
    var projectPerformance = new ProjectPerformance(48);
    var form = new ProjectPerformanceForm();
    form.setProjectCompleted(true);
    form.setStartDay("30");
    form.setStartMonth("12");
    form.setStartYear("1999");
    form.setCompletionDay("1");
    form.setCompletionMonth("1");
    form.setCompletionYear("2000");
    form.setOutturnCost("3.14");

    projectPerformanceService.updateProjectPerformance(projectPerformance, form);

    verify(projectPerformanceRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        ProjectPerformance::getId,
        ProjectPerformance::getProjectCompleted,
        ProjectPerformance::getStartDate,
        ProjectPerformance::getCompletionDate,
        ProjectPerformance::getOutturnCost
    ).containsExactly(
        projectPerformance.getId(),
        Boolean.TRUE,
        form.getStartDate().getAsLocalDate().get(),
        form.getCompletionDate().getAsLocalDate().get(),
        form.getOutturnCost().getAsBigDecimal().get()
    );
  }

  @Test
  void saveProjectPerformance_No_VerifySaves() {
    var argumentCaptor = ArgumentCaptor.forClass(ProjectPerformance.class);
    var projectPerformance = new ProjectPerformance(48);
    var form = new ProjectPerformanceForm();
    form.setProjectCompleted(false);
    form.setStartDay("30");
    form.setStartMonth("12");
    form.setStartYear("1999");
    form.setCompletionDay("1");
    form.setCompletionMonth("1");
    form.setCompletionYear("2000");
    form.setOutturnCost("3.14");

    projectPerformanceService.updateProjectPerformance(projectPerformance, form);

    verify(projectPerformanceRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        ProjectPerformance::getId,
        ProjectPerformance::getProjectCompleted,
        ProjectPerformance::getStartDate,
        ProjectPerformance::getCompletionDate,
        ProjectPerformance::getOutturnCost
    ).containsExactly(
        projectPerformance.getId(), Boolean.FALSE, null, null, null
    );
  }

  @Test
  void saveProjectPerformance_InvalidStartDate_AssertThrows() {
    var projectPerformance = new ProjectPerformance(48);
    var form = new ProjectPerformanceForm();
    form.setProjectCompleted(true);
    form.setStartDay("NaN");
    form.setStartMonth("12");
    form.setStartYear("1999");
    form.setOutturnCost("3.14");

    assertThatThrownBy(() -> projectPerformanceService.updateProjectPerformance(projectPerformance, form))
        .isInstanceOf(ClassCastException.class);

    verify(projectPerformanceRepository, never()).save(any());
  }

  @Test
  void saveProjectPerformance_InvalidCompletionDate_AssertThrows() {
    var projectPerformance = new ProjectPerformance(48);
    var form = new ProjectPerformanceForm();
    form.setProjectCompleted(true);
    form.setStartDay("30");
    form.setStartMonth("12");
    form.setStartYear("1999");
    form.setCompletionDay("NaN");
    form.setCompletionMonth("1");
    form.setCompletionYear("2000");
    form.setOutturnCost("3.14");

    assertThatThrownBy(() -> projectPerformanceService.updateProjectPerformance(projectPerformance, form))
        .isInstanceOf(ClassCastException.class);

    verify(projectPerformanceRepository, never()).save(any());
  }

  @Test
  void saveProjectPerformance_InvalidOutturnCost_AssertThrows() {
    var projectPerformance = new ProjectPerformance(48);
    var form = new ProjectPerformanceForm();
    form.setProjectCompleted(true);
    form.setStartDay("30");
    form.setStartMonth("12");
    form.setStartYear("1999");
    form.setCompletionDay("1");
    form.setCompletionMonth("1");
    form.setCompletionYear("2000");
    form.setOutturnCost("NaN");

    assertThatThrownBy(() -> projectPerformanceService.updateProjectPerformance(projectPerformance, form))
        .isInstanceOf(ClassCastException.class);

    verify(projectPerformanceRepository, never()).save(any());
  }

  @Test
  void createProjectPerformance() {
    var argumentCaptor = ArgumentCaptor.forClass(ProjectPerformance.class);
    var scapDetail = new ScapDetail();
    var form = new ProjectPerformanceForm();
    form.setProjectCompleted(true);
    form.setStartDay("30");
    form.setStartMonth("12");
    form.setStartYear("1999");
    form.setCompletionDay("1");
    form.setCompletionMonth("1");
    form.setCompletionYear("2000");
    form.setOutturnCost("3.14");

    projectPerformanceService.createProjectPerformance(scapDetail, form);

    verify(projectPerformanceRepository).save(argumentCaptor.capture());
    assertThat(argumentCaptor.getValue()).extracting(
        ProjectPerformance::getCreatedTimestamp,
        ProjectPerformance::getScapDetail,
        ProjectPerformance::getProjectCompleted,
        ProjectPerformance::getStartDate,
        ProjectPerformance::getCompletionDate,
        ProjectPerformance::getOutturnCost
    ).containsExactly(
        clock.instant(),
        scapDetail,
        Boolean.TRUE,
        form.getStartDate().getAsLocalDate().get(),
        form.getCompletionDate().getAsLocalDate().get(),
        form.getOutturnCost().getAsBigDecimal().get()
    );
  }
}
