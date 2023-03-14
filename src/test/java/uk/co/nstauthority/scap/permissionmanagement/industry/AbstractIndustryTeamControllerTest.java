package uk.co.nstauthority.scap.permissionmanagement.industry;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.boot.test.mock.mockito.MockBean;
import uk.co.nstauthority.scap.AbstractControllerTestWithSecurity;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamId;
import uk.co.nstauthority.scap.permissionmanagement.TeamMember;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberView;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewService;
import uk.co.nstauthority.scap.permissionmanagement.TeamMemberViewTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

public abstract class AbstractIndustryTeamControllerTest extends AbstractControllerTestWithSecurity {

  protected final TeamId teamId = new TeamId(UUID.randomUUID());

  protected final WebUserAccountId webUserAccountId = new WebUserAccountId(testUser.wuaId());

  protected final Team team = getTeam();

  protected final TeamMember teamMember = getTeamMember();

  @MockBean
  TeamMemberViewService teamMemberViewService;

  @BeforeEach
  void setupTeamMocks() {

    when(teamService.getTeam(any())).thenReturn(team);
    when(teamService.getRegulatorTeam()).thenReturn(getRegulatorTeam());
    when(teamMemberService.findTeamMember(any(Team.class), eq(webUserAccountId))).thenReturn(Optional.of(getTeamMember()));
    when(teamMemberService.getTeamMember(any(Team.class), eq(webUserAccountId))).thenReturn(getTeamMember());
    when(teamMemberService.isMemberOfTeam(any(TeamId.class), eq(testUser))).thenReturn(true);
    when(teamMemberViewService.getTeamMemberViewOrThrow(any(TeamMember.class))).thenReturn(getTeamMemberView());
  }

  private TeamMember getTeamMember() {
    return TeamMemberTestUtil
        .Builder()
        .withWebUserAccountId(testUser.wuaId())
        .withTeamType(TeamType.INDUSTRY)
        .withRole(IndustryTeamRole.ACCESS_MANAGER)
        .build();
  }

  private Team getTeam() {
    return TeamTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .withTeamName("Test Organisation")
        .withId(UUID.randomUUID())
        .build();
  }

  private Team getRegulatorTeam() {
    return TeamTestUtil
        .Builder()
        .withTeamType(TeamType.REGULATOR)
        .withTeamName("Test Organisation")
        .withId(UUID.randomUUID())
        .build();
  }

  private TeamMemberView getTeamMemberView() {
    return TeamMemberViewTestUtil
        .Builder()
        .withTeamType(TeamType.INDUSTRY)
        .withTeamId(teamId)
        .withRole(IndustryTeamRole.ACCESS_MANAGER)
        .withWebUserAccountId(webUserAccountId)
        .build();
  }
}
