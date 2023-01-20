package uk.co.nstauthority.scap.scap.plannedtender.activity;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.scap.RemunerationModel;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class PlannedTenderActivityServiceTest {

  @Mock
  PlannedTenderActivityRepository plannedTenderActivityRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  PlannedTenderActivityService plannedTenderActivityService;

  private PlannedTender plannedTender;

  @BeforeEach
  void setup() {
    plannedTender = new PlannedTender(null, EntityTestingUtil.dateToInstant(2000, 4, 23));
  }

  @Test
  void createPlannedTenderDetail_verifySaves() {
    var form = new PlannedTenderActivityForm();
    form.setScopeDescription("Test scope description");
    form.setEstimatedValue("2.3");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Test remuneration model");
    form.setAwardRationale("Test award rationale");
    var argumentCaptor = ArgumentCaptor.forClass(PlannedTenderActivity.class);

    plannedTenderActivityService.createPlannedTenderDetail(plannedTender, form);

    verify(plannedTenderActivityRepository).save(argumentCaptor.capture());

    var savedDetail = argumentCaptor.getValue();

    assertThat(savedDetail).extracting(
        "plannedTender",
        "scopeDescription",
        "estimatedValue",
        "remunerationModel",
        "remunerationModelName",
        "awardRationale"
    ).containsExactly(
        plannedTender,
        "Test scope description",
        BigDecimal.valueOf(2.3),
        RemunerationModel.OTHER,
        "Test remuneration model",
        "Test award rationale"
    );
  }

  @Test
  void getTenderDetailsByPlannedTender_verifyCalls() {
    var plannedTenderDetails = List.of(
        new PlannedTenderActivity(),
        new PlannedTenderActivity());
    when(plannedTenderActivityRepository.findAllByPlannedTender(plannedTender))
        .thenReturn(plannedTenderDetails);

    var returnedTenderDetails = plannedTenderActivityService.getTenderDetailsByPlannedTender(plannedTender);

    assertThat(returnedTenderDetails).isEqualTo(plannedTenderDetails);
  }

  @Test
  void hasExistingTenderDetails_noneExist_assertFalse() {
    when(plannedTenderActivityRepository.findAllByPlannedTender(plannedTender))
        .thenReturn(Collections.emptyList());

    assertFalse(plannedTenderActivityService.hasExistingTenderDetails(plannedTender));
  }

  @Test
  void hasExistingTenderDetails_someExist_assertTrue() {
    when(plannedTenderActivityRepository.findAllByPlannedTender(plannedTender))
        .thenReturn(List.of(new PlannedTenderActivity()));

    assertTrue(plannedTenderActivityService.hasExistingTenderDetails(plannedTender));
  }

  @Test
  void getPlannedTenderById_existing_assertCorrectReturn() {
    var activity = new PlannedTenderActivity(22);

    when(plannedTenderActivityRepository.findById(22)).thenReturn(Optional.of(activity));

    var returnedDetail = plannedTenderActivityService.getPlannedTenderDetailById(22);

    assertThat(returnedDetail).isEqualTo(activity);
  }

  @Test
  void getPlannedTenderById_notExisting_assertThrows() {
    when(plannedTenderActivityRepository.findById(23)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> plannedTenderActivityService.getPlannedTenderDetailById(23))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void deletePlannedTenderDetail_verifyDeletes() {
    var activity = new PlannedTenderActivity(24);

    plannedTenderActivityService.deletePlannedTenderDetail(activity);

    verify(plannedTenderActivityRepository).delete(activity);
  }

  @Test
  void updatePlannedTenderDetail_verifySaves() {
    var activity = new PlannedTenderActivity(19);
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale("Some award rationale");
    form.setEstimatedValue("2.32");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Some remuneration model");
    form.setScopeDescription("Some scope description");
    var argumentCaptor = ArgumentCaptor.forClass(PlannedTenderActivity.class);

    plannedTenderActivityService.updatePlannedTenderDetail(activity, form);

    verify(plannedTenderActivityRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        PlannedTenderActivity::getAwardRationale,
        PlannedTenderActivity::getEstimatedValue,
        PlannedTenderActivity::getRemunerationModel,
        PlannedTenderActivity::getRemunerationModelName,
        PlannedTenderActivity::getScopeDescription
    ).containsExactly(
        form.getAwardRationale().getInputValue(),
        form.getEstimatedValue().getAsBigDecimal().get(),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getScopeDescription().getInputValue()
    );
  }

  @Test
  @DisplayName("Assert that remuneration model name is null when changing type from OTHER to something else")
  void updatePlannedTenderDetail_WhenChangingRemunerationModel_VerifySaves() {
    var activity = new PlannedTenderActivity(19);
    activity.setRemunerationModelName("some remuneration model");
    activity.setRemunerationModel(RemunerationModel.OTHER);
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale("Some award rationale");
    form.setEstimatedValue("2.32");
    form.setRemunerationModel(RemunerationModel.LUMP_SUM);
    form.setRemunerationModelName("some other remuneration model");
    form.setScopeDescription("Some scope description");
    var argumentCaptor = ArgumentCaptor.forClass(PlannedTenderActivity.class);

    plannedTenderActivityService.updatePlannedTenderDetail(activity, form);

    verify(plannedTenderActivityRepository).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        PlannedTenderActivity::getId,
        PlannedTenderActivity::getAwardRationale,
        PlannedTenderActivity::getEstimatedValue,
        PlannedTenderActivity::getRemunerationModel,
        PlannedTenderActivity::getRemunerationModelName,
        PlannedTenderActivity::getScopeDescription
    ).containsExactly(
        activity.getId(),
        form.getAwardRationale().getInputValue(),
        form.getEstimatedValue().getAsBigDecimal().get(),
        form.getRemunerationModel(),
        null,
        form.getScopeDescription().getInputValue()
    );
  }

  @Test
  void updatePlannedTenderDetail_invalidEstimatedValue_verifyNeverSaves() {
    var activity = new PlannedTenderActivity(19);
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale("Some award rationale");
    form.setEstimatedValue("not a number");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Some remuneration model");
    form.setScopeDescription("Some scope description");

    assertThatThrownBy(() -> plannedTenderActivityService.updatePlannedTenderDetail(activity, form))
        .isInstanceOf(ClassCastException.class);

    verify(plannedTenderActivityRepository, never()).save(any());
  }
}
