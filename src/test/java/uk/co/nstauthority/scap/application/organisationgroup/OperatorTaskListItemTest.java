package uk.co.nstauthority.scap.application.organisationgroup;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.application.overview.ScapOverview;
import uk.co.nstauthority.scap.application.overview.ScapOverviewService;

@ExtendWith(MockitoExtension.class)
public class OperatorTaskListItemTest {

  @Mock
  private OrganisationGroupFormService organisationGroupFormService;

  @Mock
  private ScapOverviewService scapOverviewService;

  @InjectMocks
  private OperatorTaskListItem operatorTaskListItem;

  @Test
  public void isValid_verifyCallsValidator() {
    var scap = new ScapOverview(894327);
    var form = new OrganisationGroupForm();

    when(scapOverviewService.getScapById(119)).thenReturn(scap);
    when(organisationGroupFormService.getForm(scap)).thenReturn(form);
    when(organisationGroupFormService.validate(eq(form), any(BindingResult.class)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));

    var isValid = operatorTaskListItem.isValid(119);

    assertTrue(isValid);
    assertTrue(operatorTaskListItem.isVisible(0));
    verify(organisationGroupFormService, times(1)).validate(eq(form), any(BindingResult.class));
  }
}
