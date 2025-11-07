package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.springframework.data.repository.ListCrudRepository;
import org.springframework.stereotype.Repository;
import uk.co.nstauthority.scap.permissionmanagement.Team;

@Repository
public interface TeamMemberRoleRepository extends ListCrudRepository<TeamMemberRole, UUID> {

  List<TeamMemberRole> findAllByTeam(Team team);

  boolean existsByWuaIdAndTeamUuid(long wuaId, UUID teamId);

  boolean existsByWuaIdAndTeamUuidAndRoleIn(long wuaId, UUID teamId, Set<String> roles);

  List<TeamMemberRole> findAllByTeamAndWuaId(Team team, Long wuaId);

  List<TeamMemberRole> findAllByTeamInAndWuaId(List<Team> team, Long wuaId);

  void deleteAllByTeamAndWuaId(Team team, Long wuaId);

  void deleteAllByTeam(Team team);

  List<TeamMemberRole> findAllByWuaId(Long wuaId);

  List<TeamMemberRole> findAllByWuaIdIn(Collection<Long> wuaId);
}
