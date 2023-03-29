package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;
import uk.co.nstauthority.scap.scap.scap.ScapId;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class ScapOperatorServiceTest {

  @Mock
  private ScapService scapService;

  @Mock
  private ScapDetailService scapDetailService;

  @InjectMocks
  private ScapOperatorService scapOperatorService;

  private static final Scap SCAP = ScapEntityTestUtil.scapBuilder().build();
  private static final ScapDetail SCAP_DETAIL = ScapDetailEntityTestUtil.scapDetailBuilder().build();

  private final OrganisationGroupForm form = getForm();

  @Test
  void createScap() {
    when(scapService.createScap(form.getOrganisationGroupId())).thenReturn(SCAP);
    when(scapDetailService.createDraftScapDetail(SCAP)).thenReturn(SCAP_DETAIL);

    var scap = scapOperatorService.createScap(form);

    var inOrder = Mockito.inOrder(scapService, scapDetailService);

    inOrder.verify(scapService).createScap(form.getOrganisationGroupId());
    inOrder.verify(scapDetailService).createDraftScapDetail(SCAP);
    inOrder.verify(scapService).updateScapOrganisationGroup(SCAP, form.getOrganisationGroupId());
    inOrder.verify(scapDetailService).setTierOneContractor(SCAP_DETAIL, null, form);
    inOrder.verifyNoMoreInteractions();

    assertThat(scap).isEqualTo(SCAP);
  }

  @Test
  void updateScapOperator_NotTierOneOperator() {
    scapOperatorService.updateScapOperator(SCAP, SCAP_DETAIL, form);

    var inOrder = Mockito.inOrder(scapService, scapDetailService);
    inOrder.verify(scapService).updateScapOrganisationGroup(SCAP, form.getOrganisationGroupId());
    inOrder.verify(scapDetailService).setTierOneContractor(SCAP_DETAIL, null, form);
  }

  @Test
  void updateScapOperator_IsTierOneOperator() {
    var parentScap = ScapEntityTestUtil.scapBuilder()
        .withScapId(ScapId.valueOf(1))
        .build();
    form.setIsTierOneContractor(true);
    form.setParentScapId(parentScap.getId());

    when(scapService.getScapById(parentScap.getId())).thenReturn(parentScap);

    scapOperatorService.updateScapOperator(SCAP, SCAP_DETAIL, form);

    var inOrder = Mockito.inOrder(scapService, scapDetailService);
    inOrder.verify(scapService).updateScapOrganisationGroup(SCAP, form.getOrganisationGroupId());
    inOrder.verify(scapDetailService).setTierOneContractor(SCAP_DETAIL, parentScap, form);
  }

  private OrganisationGroupForm getForm() {
    var form = new OrganisationGroupForm();
    form.setParentScapId(55);
    form.setIsTierOneContractor(false);
    return form;
  }
}