package uk.co.nstauthority.scap.permissionmanagement.regulator;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.boot.test.mock.mockito.MockBean;
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.ServiceUserDetailTestUtil;
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

public abstract class AbstractRegulatorTeamControllerTest extends AbstractControllerTest {

  protected final ServiceUserDetail user = getUser();

  protected final TeamId teamId = new TeamId(UUID.randomUUID());

  protected final WebUserAccountId webUserAccountId = new WebUserAccountId(user.wuaId());

  protected final Team team = getTeam();

  protected final TeamMember teamMember = getTeamMember();

  @MockBean
  TeamMemberViewService teamMemberViewService;

  @BeforeEach
  void setupTeamMocks() {

    when(teamService.getTeam(any())).thenReturn(team);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(any(), eq(TeamType.REGULATOR))).thenReturn(List.of(team));
    when(teamService.getTeamsThatUserBelongsTo(user)).thenReturn(List.of(team));
    when(userDetailService.getUserDetail()).thenReturn(user);
    when(teamMemberService.findTeamMember(any(Team.class), any())).thenReturn(Optional.of(teamMember));
    when(teamMemberService.getTeamMember(any(Team.class), eq(webUserAccountId))).thenReturn(teamMember);
    when(teamMemberService.isMemberOfTeam(any(TeamId.class), eq(user))).thenReturn(true);
    when(teamMemberViewService.getTeamMemberView(any(TeamMember.class))).thenReturn(getTeamMemberView());
  }

  private ServiceUserDetail getUser() {
    return ServiceUserDetailTestUtil.Builder()
        .withWuaId(100L)
        .build();
  }

  private TeamMember getTeamMember() {
    return TeamMemberTestUtil
        .Builder()
        .withWebUserAccountId(user.wuaId())
        .withTeamType(TeamType.REGULATOR)
        .withRoles(Set.of(RegulatorTeamRole.ACCESS_MANAGER, RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER))
        .build();
  }

  private Team getTeam() {
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
        .withTeamType(TeamType.REGULATOR)
        .withTeamId(teamId)
        .withRoles(Set.of(RegulatorTeamRole.ACCESS_MANAGER, RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER))
        .withWebUserAccountId(webUserAccountId)
        .build();
  }
}
