package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.detail.ScapDetailEntityTestUtil;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@ExtendWith(MockitoExtension.class)
class OperatorTaskListItemTest {

  @Mock
  private OrganisationGroupFormService organisationGroupFormService;

  @Mock
  private ScapService scapService;

  @Mock
  private ScapDetailService scapDetailService;

  @InjectMocks
  private OperatorTaskListItem operatorTaskListItem;

  @Test
  void isValid_verifyCallsValidator() {
    var scap = new Scap(894327);
    var scapDetail = ScapDetailEntityTestUtil.scapDetailBuilder()
        .withScap(scap)
        .build();
    var form = new OrganisationGroupForm();
    var scapId = 119;

    when(scapService.getScapById(scapId)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(organisationGroupFormService.getForm(scapDetail)).thenReturn(form);
    when(organisationGroupFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));

    var isValid = operatorTaskListItem.isValid(scapId);

    assertTrue(isValid);
    assertTrue(operatorTaskListItem.isVisible(scapId));
    verify(organisationGroupFormService).validate(eq(form), any(BindingResult.class));
  }
}
