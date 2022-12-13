package uk.co.nstauthority.scap.permissionmanagement.teams;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;
import uk.co.nstauthority.scap.permissionmanagement.Team;

@Repository
interface TeamMemberRoleRepository extends CrudRepository<TeamMemberRole, UUID> {

  List<TeamMemberRole> findAllByTeam(Team team);

  boolean existsByWuaIdAndTeamUuid(long wuaId, UUID teamId);

  boolean existsByWuaIdAndTeamUuidAndRoleIn(long wuaId, UUID teamId, Set<String> roles);

  List<TeamMemberRole> findAllByTeamAndWuaId(Team team, Long wuaId);

  void deleteAllByTeamAndWuaId(Team team, Long wuaId);

}
