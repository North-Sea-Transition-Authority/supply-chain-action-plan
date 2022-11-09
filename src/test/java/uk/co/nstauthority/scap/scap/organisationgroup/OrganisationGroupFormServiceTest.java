package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;

@ExtendWith(MockitoExtension.class)
class OrganisationGroupFormServiceTest {

  @Mock
  private OrganisationGroupFormValidator organisationGroupFormValidator;

  @InjectMocks
  private OrganisationGroupFormService organisationGroupFormService;


  @Test
  void validate_verifyMethodCall() {
    var form = new OrganisationGroupForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");

    organisationGroupFormService.validate(form, bindingResult);

    verify(organisationGroupFormValidator).validate(form, bindingResult);
  }

}
