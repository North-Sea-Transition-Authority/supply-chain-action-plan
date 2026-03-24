package uk.co.nstauthority.scap.energyportal.user;

import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import static org.assertj.core.api.Assertions.assertThat;

@ExtendWith(MockitoExtension.class)
class OracleAllowedDomainServiceTest {

  @Mock
  private Team team;

  @InjectMocks
  private OracleAllowedDomainService oracleAllowedDomainService;

  @ParameterizedTest
  @ValueSource(strings = {"test.com", "gov.uk"})
  void isAllowedDomain_AlwaysReturnsTrue(String domain) {
    boolean result = oracleAllowedDomainService.isAllowedDomain(domain, team);

    assertThat(result).isTrue();
  }
}
