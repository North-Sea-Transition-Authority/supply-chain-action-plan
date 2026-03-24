package uk.co.nstauthority.scap.energyportal.organisationgroup;

import org.junit.jupiter.api.Test;
import java.util.List;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroupEmailDomain;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class OrganisationGroupDtoTest {

  @Mock
  private OrganisationGroupEmailDomain emailDomain;
  @Mock
  private OrganisationGroup organisationGroup;

  @Test
  void from_ShouldMapAllFields() {

    when(organisationGroup.getOrganisationGroupId()).thenReturn(101);
    when(organisationGroup.getName()).thenReturn("Energy Corp");
    when(emailDomain.getDomain()).thenReturn("energy.gov.uk");
    when(organisationGroup.getEmailDomains()).thenReturn(List.of(emailDomain));

    OrganisationGroupDto result = OrganisationGroupDto.from(organisationGroup);

    assertThat(result.organisationGroupId()).isEqualTo(101);
    assertThat(result.organisationGroupName()).isEqualTo("Energy Corp");
    assertThat(result.emailDomains())
        .hasSize(1)
        .containsExactly("energy.gov.uk");
  }
}