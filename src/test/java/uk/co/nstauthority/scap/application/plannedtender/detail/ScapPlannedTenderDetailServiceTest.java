package uk.co.nstauthority.scap.application.plannedtender.detail;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.RemunerationModel;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.error.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.utils.EntityTestingUtil;

@ExtendWith(MockitoExtension.class)
class ScapPlannedTenderDetailServiceTest {

  @Mock
  ScapPlannedTenderDetailRepository scapPlannedTenderDetailRepository;

  @Mock
  Clock clock = Clock.fixed(Instant.ofEpochSecond(1667576106), ZoneId.systemDefault());

  @InjectMocks
  ScapPlannedTenderDetailService scapPlannedTenderDetailService;

  private ScapPlannedTender scapPlannedTender;

  @BeforeEach
  void setup() {
    scapPlannedTender = new ScapPlannedTender(null, EntityTestingUtil.dateToInstant(2000, 4, 23));
  }

  @Test
  void createPlannedTenderDetail_verifySaves() {
    var form = new ScapPlannedTenderDetailForm();
    form.setScopeDescription("Test scope description");
    form.setEstimatedValue("2.3");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Test remuneration model");
    form.setAwardRationale("Test award rationale");
    var argumentCaptor = ArgumentCaptor.forClass(ScapPlannedTenderDetail.class);

    scapPlannedTenderDetailService.createPlannedTenderDetail(scapPlannedTender, form);

    verify(scapPlannedTenderDetailRepository, times(1)).save(argumentCaptor.capture());

    var savedDetail = argumentCaptor.getValue();

    assertThat(savedDetail).extracting(
        "plannedTender",
        "scopeDescription",
        "estimatedValue",
        "remunerationModel",
        "remunerationModelName",
        "awardRationale"
    ).containsExactly(
        scapPlannedTender,
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
        new ScapPlannedTenderDetail(),
        new ScapPlannedTenderDetail());
    when(scapPlannedTenderDetailRepository.findAllByPlannedTender(scapPlannedTender))
        .thenReturn(plannedTenderDetails);

    var returnedTenderDetails = scapPlannedTenderDetailService.getTenderDetailsByPlannedTender(scapPlannedTender);

    assertThat(returnedTenderDetails).isEqualTo(plannedTenderDetails);
  }

  @Test
  void hasExistingTenderDetails_noneExist_assertFalse() {
    when(scapPlannedTenderDetailRepository.findAllByPlannedTender(scapPlannedTender))
        .thenReturn(Collections.emptyList());

    assertFalse(scapPlannedTenderDetailService.hasExistingTenderDetails(scapPlannedTender));
  }

  @Test
  void hasExistingTenderDetails_someExist_assertTrue() {
    when(scapPlannedTenderDetailRepository.findAllByPlannedTender(scapPlannedTender))
        .thenReturn(List.of(new ScapPlannedTenderDetail()));

    assertTrue(scapPlannedTenderDetailService.hasExistingTenderDetails(scapPlannedTender));
  }

  @Test
  void getPlannedTenderById_existing_assertCorrectReturn() {
    var detail = new ScapPlannedTenderDetail(22);

    when(scapPlannedTenderDetailRepository.findById(22)).thenReturn(Optional.of(detail));

    var returnedDetail = scapPlannedTenderDetailService.getPlannedTenderDetailById(22);

    assertThat(returnedDetail).isEqualTo(detail);
  }

  @Test
  void getPlannedTenderById_notExisting_assertThrows() {
    when(scapPlannedTenderDetailRepository.findById(23)).thenReturn(Optional.empty());

    assertThatThrownBy(() -> scapPlannedTenderDetailService.getPlannedTenderDetailById(23))
        .isInstanceOf(ScapEntityNotFoundException.class);
  }

  @Test
  void deletePlannedTenderDetail_verifyDeletes() {
    var detail = new ScapPlannedTenderDetail(24);

    scapPlannedTenderDetailService.deletePlannedTenderDetail(detail);

    verify(scapPlannedTenderDetailRepository, times(1)).delete(detail);
  }

  @Test
  void updatePlannedTenderDetail_verifySaves() {
    var detail = new ScapPlannedTenderDetail(19);
    var form = new ScapPlannedTenderDetailForm();
    form.setAwardRationale("Some award rationale");
    form.setEstimatedValue("2.32");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Some remuneration model");
    form.setScopeDescription("Some scope description");
    var argumentCaptor = ArgumentCaptor.forClass(ScapPlannedTenderDetail.class);

    scapPlannedTenderDetailService.updatePlannedTenderDetail(detail, form);

    verify(scapPlannedTenderDetailRepository, times(1)).save(argumentCaptor.capture());

    assertThat(argumentCaptor.getValue()).extracting(
        ScapPlannedTenderDetail::getAwardRationale,
        ScapPlannedTenderDetail::getEstimatedValue,
        ScapPlannedTenderDetail::getRemunerationModel,
        ScapPlannedTenderDetail::getRemunerationModelName,
        ScapPlannedTenderDetail::getScopeDescription
    ).containsExactly(
        form.getAwardRationale().getInputValue(),
        form.getEstimatedValue().getAsBigDecimal().get(),
        form.getRemunerationModel(),
        form.getRemunerationModelName().getInputValue(),
        form.getScopeDescription().getInputValue()
    );
  }

  @Test
  void updatePlannedTenderDetail_invalidEstimatedValue_verifyNeverSaves() {
    var detail = new ScapPlannedTenderDetail(19);
    var form = new ScapPlannedTenderDetailForm();
    form.setAwardRationale("Some award rationale");
    form.setEstimatedValue("not a number");
    form.setRemunerationModel(RemunerationModel.OTHER);
    form.setRemunerationModelName("Some remuneration model");
    form.setScopeDescription("Some scope description");

    assertThatThrownBy(() -> scapPlannedTenderDetailService.updatePlannedTenderDetail(detail, form))
        .isInstanceOf(ClassCastException.class);

    verify(scapPlannedTenderDetailRepository, never()).save(any());
  }
}
