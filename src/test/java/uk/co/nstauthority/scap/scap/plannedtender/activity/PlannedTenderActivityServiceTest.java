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
import java.time.LocalDate;
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

@SuppressWarnings("OptionalGetWithoutIsPresent")
@ExtendWith(MockitoExtension.class)
class PlannedTenderActivityServiceTest {

  @Mock
  PlannedTenderActivityRepository plannedTenderActivityRepository;

  @Mock
  Clock clock;

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
    var scopeDescription = "Test scope description";
    var estimatedValue = BigDecimal.valueOf(2.3);
    var remunerationModel = RemunerationModel.OTHER;
    var remunerationModelName = "Test remuneration model";
    var awardRationale = "Test award rationale";
    var startDate = LocalDate.of(2000, 1, 1);
    var awardDate = LocalDate.of(2000, 2, 1);
    var timestamp = Instant.ofEpochSecond(1667576106);
    form.setScopeDescription(scopeDescription);
    form.setEstimatedValue(estimatedValue.toPlainString());
    form.setRemunerationModel(remunerationModel);
    form.setRemunerationModelName(remunerationModelName);
    form.setAwardRationale(awardRationale);
    form.setIndicativeActualTenderStartDate(startDate);
    form.setIndicativeContractAwardDate(awardDate);
    var argumentCaptor = ArgumentCaptor.forClass(PlannedTenderActivity.class);

    when(clock.instant()).thenReturn(timestamp);

    plannedTenderActivityService.createPlannedTenderDetail(plannedTender, form);

    verify(plannedTenderActivityRepository).save(argumentCaptor.capture());

    var savedDetail = argumentCaptor.getValue();

    assertThat(savedDetail).extracting(
        PlannedTenderActivity::getPlannedTender,
        PlannedTenderActivity::getScopeDescription,
        PlannedTenderActivity::getEstimatedValue,
        PlannedTenderActivity::getRemunerationModel,
        PlannedTenderActivity::getRemunerationModelName,
        PlannedTenderActivity::getAwardRationale,
        PlannedTenderActivity::getCreatedTimestamp,
        PlannedTenderActivity::getExpectedActualTenderStartDate,
        PlannedTenderActivity::getExpectedContractAwardDate
    ).containsExactly(
        plannedTender,
        scopeDescription,
        estimatedValue,
        remunerationModel,
        remunerationModelName,
        awardRationale,
        timestamp,
        startDate,
        awardDate
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
    var startDate = LocalDate.of(2000, 1, 1);
    var awardDate = LocalDate.of(2000, 2, 1);
    form.setIndicativeActualTenderStartDate(startDate);
    form.setIndicativeContractAwardDate(awardDate);
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
        PlannedTenderActivity::getScopeDescription,
        PlannedTenderActivity::getExpectedActualTenderStartDate,
        PlannedTenderActivity::getExpectedContractAwardDate
    ).containsExactly(
        form.getAwardRationale().getInputValue(),
        form.getEstimatedValue().getAsBigDecimal().get(),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getScopeDescription().getInputValue(),
        startDate,
        awardDate
    );
  }

  @Test
  @DisplayName("Assert that remuneration model name is null when changing type from OTHER to something else")
  void updatePlannedTenderDetail_WhenChangingRemunerationModel_VerifySaves() {
    var activity = new PlannedTenderActivity(19);
    activity.setRemunerationModelName("some remuneration model");
    activity.setRemunerationModel(RemunerationModel.OTHER);
    var form = new PlannedTenderActivityForm();
    var startDate = LocalDate.of(2000, 1, 1);
    var awardDate = LocalDate.of(2000, 2, 1);
    form.setIndicativeActualTenderStartDate(startDate);
    form.setIndicativeContractAwardDate(awardDate);
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
        PlannedTenderActivity::getScopeDescription,
        PlannedTenderActivity::getExpectedActualTenderStartDate,
        PlannedTenderActivity::getExpectedContractAwardDate
    ).containsExactly(
        activity.getId(),
        form.getAwardRationale().getInputValue(),
        form.getEstimatedValue().getAsBigDecimal().get(),
        form.getRemunerationModel(),
        null,
        form.getScopeDescription().getInputValue(),
        startDate,
        awardDate
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

  @Test
  void updatePlannedTenderDetail_InvalidActualTenderStartDate_VerifyNeverSaves() {
    var activity = new PlannedTenderActivity(19);
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale("Some award rationale");
    form.setEstimatedValue("2");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Some remuneration model");
    form.setScopeDescription("Some scope description");
    form.setIndicativeActualTenderStartDate(null);
    form.setIndicativeContractAwardDate(LocalDate.of(2000, 1, 1));

    var expectedErrorMessage =
        "Update failed for planned tender activity with ID %d, as indicativeActualTenderStartDate is not a LocalDate"
        .formatted(activity.getId());

    assertThatThrownBy(() -> plannedTenderActivityService.updatePlannedTenderDetail(activity, form))
        .isInstanceOf(ClassCastException.class)
        .hasMessage(expectedErrorMessage);

    verify(plannedTenderActivityRepository, never()).save(any());
  }

  @Test
  void updatePlannedTenderDetail_InvalidContractAwardDate_VerifyNeverSaves() {
    var activity = new PlannedTenderActivity(19);
    var form = new PlannedTenderActivityForm();
    form.setAwardRationale("Some award rationale");
    form.setEstimatedValue("2");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Some remuneration model");
    form.setScopeDescription("Some scope description");
    form.setIndicativeActualTenderStartDate(LocalDate.of(2000, 1, 1));
    form.setIndicativeContractAwardDate(null);

    var expectedErrorMessage =
        "Update failed for planned tender activity with ID %d, as indicativeContractAwardDate is not a LocalDate"
            .formatted(activity.getId());

    assertThatThrownBy(() -> plannedTenderActivityService.updatePlannedTenderDetail(activity, form))
        .isInstanceOf(ClassCastException.class)
        .hasMessage(expectedErrorMessage);

    verify(plannedTenderActivityRepository, never()).save(any());
  }

}
