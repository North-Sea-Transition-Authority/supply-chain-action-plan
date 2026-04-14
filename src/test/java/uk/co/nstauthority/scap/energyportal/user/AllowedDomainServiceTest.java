package uk.co.nstauthority.scap.energyportal.user;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.energyportal.organisationgroup.OrganisationGroupDto;
import uk.co.nstauthority.scap.energyportal.organisationgroup.OrganisationGroupQueryService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@ExtendWith(MockitoExtension.class)
class AllowedDomainServiceTest {

  private static final String USER_EMAIL = "user@example.com";

  @Mock
  private OrganisationGroupQueryService organisationGroupQueryService;

  @InjectMocks
  private AllowedDomainService allowedDomainService;


  @ParameterizedTest
  @MethodSource("provideDomainIsAllowedCombinations")
  void getRegulatorOrganisationGroup(String domain, boolean isAllowed) {
    var secondaryRegulatorTeam = new Team(UUID.randomUUID());
    secondaryRegulatorTeam.setTeamType(TeamType.REGULATOR);
    secondaryRegulatorTeam.setDisplayName("regulator team");

    var orgGroup = new OrganisationGroupDto(1, "group1", List.of(domain));

    when(organisationGroupQueryService.getRegulatorOrganisationGroup()).thenReturn(
        Optional.of(orgGroup)
    );

    assertThat(allowedDomainService.isAllowedDomain(USER_EMAIL, secondaryRegulatorTeam)).isEqualTo(isAllowed);
  }

  @ParameterizedTest
  @MethodSource("provideDomainIsAllowedCombinations")
  void isAllowedDomain_organisation(String domain, boolean isAllowed) {
    var organisationTeam = new Team(UUID.randomUUID());
    organisationTeam.setTeamType(TeamType.INDUSTRY);
    organisationTeam.setDisplayName("organisation team");
    organisationTeam.setEnergyPortalOrgGroupId(1);

    var orgGroup = new OrganisationGroupDto(1, "org1", List.of(domain));

    when(organisationGroupQueryService.getOrganisationGroupById(organisationTeam.getEnergyPortalOrgGroupId())).thenReturn(
        Optional.of(orgGroup)
    );

    assertThat(allowedDomainService.isAllowedDomain(USER_EMAIL, organisationTeam)).isEqualTo(isAllowed);
  }

  @ParameterizedTest
  @EnumSource(TeamType.class)
  void isAllowedDomain_ShouldSupportAllTeamTypes(TeamType teamType) {
    Team team = new Team(UUID.randomUUID());
    team.setTeamType(teamType);
    team.setEnergyPortalOrgGroupId(1);

    lenient().when(organisationGroupQueryService.getOrganisationGroupById(anyInt()))
        .thenReturn(Optional.empty());
    lenient().when(organisationGroupQueryService.getRegulatorOrganisationGroup())
        .thenReturn(Optional.empty());

    assertDoesNotThrow(() ->
        allowedDomainService.isAllowedDomain(USER_EMAIL, team)
    );
  }

  private static Stream<Arguments> provideDomainIsAllowedCombinations() {
    return Stream.of(
        Arguments.of("example.com", true),
        Arguments.of("domain.com", false)
    );
  }
}
