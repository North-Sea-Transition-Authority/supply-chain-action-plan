package uk.co.nstauthority.scap;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;

import java.util.Optional;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.boot.test.mock.mockito.MockBean;
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
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapId;

public abstract class AbstractScapSubmitterControllerTest extends AbstractControllerTest {

  protected final TeamId teamId = new TeamId(UUID.randomUUID());

  protected final WebUserAccountId webUserAccountId = new WebUserAccountId(testUser.wuaId());

  protected final static ScapId SCAP_ID = new ScapId(1111);

  protected final Scap scap = getScap();

  protected final Team team = getTeam();

  protected final TeamMember teamMember = getTeamMember();

  @MockBean
  TeamMemberViewService teamMemberViewService;

  @BeforeEach
  void setupSubmitterMocks() {
    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(scapService.getScapById(SCAP_ID.scapId())).thenReturn(scap);
    when(teamService.getByEnergyPortalOrgGroupId(anyInt())).thenReturn(team);
    when(userDetailService.getUserDetail()).thenReturn(testUser);
    when(teamMemberService.findTeamMember(any(Team.class), eq(webUserAccountId))).thenReturn(Optional.of(getTeamMember()));
    when(teamMemberService.getTeamMember(any(Team.class), eq(webUserAccountId))).thenReturn(getTeamMember());
  }

  private TeamMember getTeamMember() {
    return TeamMemberTestUtil
        .Builder()
        .withWebUserAccountId(testUser.wuaId())
        .withTeamType(TeamType.INDUSTRY)
        .withRole(IndustryTeamRole.SCAP_SUBMITTER)
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

  private Scap getScap() {
    var scap = new Scap(1111);
    scap.setReference("Test Scap");
    scap.setOrganisationGroupId(1000);
    return scap;
  }
}
