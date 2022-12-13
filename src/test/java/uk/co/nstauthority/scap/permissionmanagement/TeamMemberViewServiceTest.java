package uk.co.nstauthority.scap.permissionmanagement;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.assertj.core.groups.Tuple;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamView;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;

@ExtendWith(MockitoExtension.class)
class TeamMemberViewServiceTest {

  @Mock
  private EnergyPortalUserService energyPortalUserService;

  @Mock
  private TeamMemberService teamMemberService;

  @InjectMocks
  private TeamMemberViewService teamMemberViewService;

  @Test
  void getUserViewsForTeam() {
    var team = new Team(UUID.randomUUID());
    var wuaId = 100L;

    var teamView = TeamView.fromTeam(team);

    var teamMember = new TeamMember(new WebUserAccountId(wuaId), teamView,
        Set.of(RegulatorTeamRole.ACCESS_MANAGER, RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER));

    when(teamMemberService.getTeamMembers(team))
        .thenReturn(List.of(teamMember));

    var portalUser = EnergyPortalUserDtoTestUtil.Builder()
        .withWebUserAccountId(wuaId)
        .withTitle("Dr")
        .withForename("John")
        .withSurname("Smith")
        .withEmailAddress("john.smith@test.org")
        .withPhoneNumber("telephone")
        .build();

    when(energyPortalUserService.findByWuaIds(List.of(new WebUserAccountId(wuaId))))
        .thenReturn(List.of(portalUser));

    var result = teamMemberViewService.getTeamMemberViewsForTeam(team);

    assertThat(result).extracting(
        TeamMemberView::wuaId,
        TeamMemberView::teamView,
        TeamMemberView::title,
        TeamMemberView::firstName,
        TeamMemberView::lastName,
        TeamMemberView::contactEmail,
        TeamMemberView::contactNumber,
        TeamMemberView::teamRoles
    ).containsExactly(
        Tuple.tuple(
            new WebUserAccountId(wuaId),
            teamView,
            "Dr",
            "John",
            "Smith",
            "john.smith@test.org",
            "telephone",
            Set.of(RegulatorTeamRole.ACCESS_MANAGER, RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER)
        )
    );

  }

  @Test
  void getUserViewForTeamMember() {
    var team = new Team(UUID.randomUUID());
    var wuaId = 100L;

    var teamView = TeamView.fromTeam(team);

    var teamMember = new TeamMember(new WebUserAccountId(wuaId), teamView,
        Set.of(RegulatorTeamRole.ACCESS_MANAGER, RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER));

    var portalUser = EnergyPortalUserDtoTestUtil.Builder()
        .withWebUserAccountId(wuaId)
        .withTitle("Dr")
        .withForename("John")
        .withSurname("Smith")
        .withEmailAddress("john.smith@test.org")
        .withPhoneNumber("telephone")
        .build();

    when(energyPortalUserService.findByWuaIds(List.of(new WebUserAccountId(wuaId))))
        .thenReturn(List.of(portalUser));

    var result = teamMemberViewService.getTeamMemberView(teamMember);

    assertTrue(result.isPresent());
    assertThat(result.get()).extracting(
        TeamMemberView::wuaId,
        TeamMemberView::teamView,
        TeamMemberView::title,
        TeamMemberView::firstName,
        TeamMemberView::lastName,
        TeamMemberView::contactEmail,
        TeamMemberView::contactNumber,
        TeamMemberView::teamRoles
    ).containsExactly(
        new WebUserAccountId(wuaId),
        teamView,
        "Dr",
        "John",
        "Smith",
        "john.smith@test.org",
        "telephone",
        Set.of(RegulatorTeamRole.ACCESS_MANAGER, RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER)
    );
  }
}