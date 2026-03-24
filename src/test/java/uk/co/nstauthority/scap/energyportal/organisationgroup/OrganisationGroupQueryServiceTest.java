package uk.co.nstauthority.scap.energyportal.organisationgroup;


import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportal.starter.configuration.WellKnownOrganisationGroupsConfigurationProperties;
import uk.co.fivium.energyportalapi.client.RequestPurpose;
import uk.co.fivium.energyportalapi.client.organisation.OrganisationApi;
import uk.co.fivium.energyportalapi.generated.client.OrganisationGroupProjectionRoot;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;

@ExtendWith(MockitoExtension.class)
class OrganisationGroupQueryServiceTest {

  @Mock
  private OrganisationApi organisationApi;

  @Mock
  private WellKnownOrganisationGroupsConfigurationProperties wellKnownGroups;

  @Mock
  private WellKnownOrganisationGroupsConfigurationProperties.WellKnownOrgGroup nsta;

  @InjectMocks
  private OrganisationGroupQueryService organisationGroupQueryService;

  @Test
  void getOrganisationGroupById_verifyCallsApiWithCorrectParameters() {
    var argumentCaptor = ArgumentCaptor
        .forClass(OrganisationGroupProjectionRoot.class);
    var organisationGroup = new OrganisationGroup(
        1,
        "Royal Dutch Shell",
        "Shell",
        "shell.com",
        "ACTIVE",
        Collections.emptyList(),
        Collections.emptyList());

    when(organisationApi.findOrganisationGroup(
        eq(organisationGroup.getOrganisationGroupId()),
        any(OrganisationGroupProjectionRoot.class),
        any(RequestPurpose.class)))
        .thenReturn(Optional.of(organisationGroup));

    organisationGroupQueryService.getOrganisationGroupById(1);

    verify(organisationApi).findOrganisationGroup(
        eq(organisationGroup.getOrganisationGroupId()),
        argumentCaptor.capture(),
        any(RequestPurpose.class));

    assertThat(argumentCaptor.getValue().getFields())
        .containsOnlyKeys(
            "organisationGroupId",
            "name",
            "emailDomains"
        );
  }

  @Test
  void getRegulatorOrganisationGroup() {
    var expectedId = Math.toIntExact(10001L);

    when(wellKnownGroups.nsta()).thenReturn(nsta);
    when(nsta.idAsInteger()).thenReturn(expectedId);

    organisationGroupQueryService.getRegulatorOrganisationGroup();

    verify(organisationApi).findOrganisationGroup(
        eq(expectedId),
        any(),
        any(RequestPurpose.class)
    );
  }
}