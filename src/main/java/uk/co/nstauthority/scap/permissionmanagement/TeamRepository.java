package uk.co.nstauthority.scap.permissionmanagement;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TeamRepository extends CrudRepository<Team, UUID> {

  Optional<Team> findByUuid(UUID uuid);

  @Query(
      """
      SELECT DISTINCT tmr.team
      FROM TeamMemberRole tmr
      WHERE tmr.wuaId = :wuaId
      AND tmr.team.teamType = :teamType
      """
  )
  List<Team> findAllTeamsOfTypeThatUserIsMemberOf(Long wuaId, TeamType teamType);

  @Query(
      """
      SELECT DISTINCT tmr.team
      FROM TeamMemberRole tmr
      WHERE tmr.wuaId = :wuaId
      """
  )
  List<Team> findAllTeamsThatUserIsMemberOf(Long wuaId);

  Optional<Team> findByEnergyPortalOrgGroupId(int energyPortalOrgGroupId);

  Team getTeamByTeamType(TeamType teamType);

}
