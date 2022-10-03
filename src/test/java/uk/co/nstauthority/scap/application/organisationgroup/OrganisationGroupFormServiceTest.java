package uk.co.nstauthority.scap.application.organisationgroup;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;

@ExtendWith(MockitoExtension.class)
public class OrganisationGroupFormServiceTest {

  @Mock
  private OrganisationGroupFormValidator organisationGroupFormValidator;

  @InjectMocks
  private OrganisationGroupFormService organisationGroupFormService;


  @Test
  public void validate_verifyMethodCall() {
    var form = new OrganisationGroupForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    organisationGroupFormService.validate(form, bindingResult);

    verify(organisationGroupFormValidator, times(1)).validate(form, bindingResult);
  }

}
