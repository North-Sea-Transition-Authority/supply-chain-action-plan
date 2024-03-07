package uk.co.nstauthority.scap.permissionmanagement;

import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserDto;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.error.exception.ScapEntityNotFoundException;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;

@Service
public class TeamMemberViewService {

  private final TeamMemberService teamMemberService;
  private final EnergyPortalUserService energyPortalUserService;

  @Autowired
  public TeamMemberViewService(TeamMemberService teamMemberService, EnergyPortalUserService energyPortalUserService) {
    this.teamMemberService = teamMemberService;
    this.energyPortalUserService = energyPortalUserService;
  }

  public List<TeamMemberView> findTeamMemberViewsForTeam(Team team) {
    var members = teamMemberService.getTeamMembers(team);
    return createUserViewsFromTeamMembers(members);
  }

  public Optional<TeamMemberView> findTeamMemberView(TeamMember teamMember) {
    return createUserViewsFromTeamMembers(List.of(teamMember))
        .stream()
        .findFirst();
  }

  public TeamMemberView getTeamMemberView(TeamMember teamMember) {
    return createUserViewsFromTeamMembers(List.of(teamMember))
        .stream()
        .findFirst()
        .orElseThrow(() -> new ScapEntityNotFoundException(
            "Could not find View for User with ID: %s".formatted(teamMember.wuaId().id())));
  }

  private List<TeamMemberView> createUserViewsFromTeamMembers(Collection<TeamMember> teamMembers) {

    // extract list of WUA to lookup
    var webUserAccountIds = teamMembers
        .stream()
        .map(TeamMember::wuaId)
        .toList();

    // Create map of Energy Portal users with WUA as the key for ease of lookup
    Map<WebUserAccountId, EnergyPortalUserDto> energyPortalUsers = energyPortalUserService.findByWuaIds(webUserAccountIds)
        .stream()
        .collect(Collectors.toMap(energyPortalUser ->
            new WebUserAccountId(energyPortalUser.webUserAccountId()),
            Function.identity())
        );

    return teamMembers
        .stream()
        .map(teamMember -> createTeamMemberView(teamMember, energyPortalUsers))
        .sorted(Comparator.comparing(TeamMemberView::firstName).thenComparing(TeamMemberView::lastName))
        .toList();
  }

  private TeamMemberView createTeamMemberView(TeamMember teamMember,
                                              Map<WebUserAccountId, EnergyPortalUserDto> energyPortalUsers) {

    if (energyPortalUsers.containsKey(teamMember.wuaId())) {

      var energyPortalUser = energyPortalUsers.get(teamMember.wuaId());

      var roles = teamMember.roles()
          .stream()
          .sorted(Comparator.comparing(TeamRole::getDisplayOrder))
          .collect(Collectors.toCollection(LinkedHashSet::new));

      return new TeamMemberView(
          teamMember.wuaId(),
          teamMember.teamView(),
          energyPortalUser.title(),
          energyPortalUser.forename(),
          energyPortalUser.surname(),
          energyPortalUser.emailAddress(),
          energyPortalUser.telephoneNumber(),
          roles
      );
    } else {
      throw new IllegalArgumentException(
         "Did not find an Energy Portal User with WUA ID %s when converting team members"
             .formatted(teamMember.wuaId())
     );
    }
  }
}
