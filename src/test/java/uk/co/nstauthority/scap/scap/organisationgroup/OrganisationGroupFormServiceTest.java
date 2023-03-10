package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;
import uk.co.nstauthority.scap.scap.scap.ScapEntityTestUtil;

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

  @Test
  void getForm() {
    var orgGroupId = 5;
    var scap = ScapEntityTestUtil.scapBuilder()
        .withOrganisationGroupId(orgGroupId)
        .build();

    var form = organisationGroupFormService.getForm(scap);

    assertThat(form).extracting(
        OrganisationGroupForm::getOrganisationGroupId
    ).isEqualTo(
        orgGroupId
    );
  }

}
