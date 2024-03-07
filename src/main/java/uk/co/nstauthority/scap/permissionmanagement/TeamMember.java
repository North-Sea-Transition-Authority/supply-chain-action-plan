package uk.co.nstauthority.scap.permissionmanagement;

import java.util.Set;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamView;

public record TeamMember(WebUserAccountId wuaId, TeamView teamView, Set<TeamRole> roles) {
}
