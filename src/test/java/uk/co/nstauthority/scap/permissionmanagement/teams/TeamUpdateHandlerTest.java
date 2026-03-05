package uk.co.nstauthority.scap.permissionmanagement.teams;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportal.starter.organisationgroup.EnergyPortalOrganisationGroupEvent;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamTestUtil;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;

@ExtendWith(MockitoExtension.class)
class TeamUpdateHandlerTest {

  private static final Team TEAM = TeamTestUtil
      .Builder()
      .withTeamType(TeamType.INDUSTRY)
      .withTeamName("Test Organisation")
      .withOrgGroupId(1)
      .build();

  @Mock
  private TeamService teamService;

  @InjectMocks
  private TeamUpdateHandler teamUpdateHandler;

  private EnergyPortalOrganisationGroupEvent event;

  @Test
  void onEnergyPortalOrganisationGroupEvent_whenGroupCreated_thenDoNothing(){
    createEvent("Test Organisation", true);

    teamUpdateHandler.onEnergyPortalOrganisationGroupEvent(event);

    verifyNoInteractions(teamService);
  }

  @Test
  void onEnergyPortalOrganisationGroupEvent_whenGroupUpdated_andNoTeam_thenDoNothing(){
    createEvent("Test Organisation", false);

    when(teamService.findByEnergyPortalOrgGroupId(anyInt())).thenReturn(Optional.empty());

    teamUpdateHandler.onEnergyPortalOrganisationGroupEvent(event);

    verify(teamService).findByEnergyPortalOrgGroupId(Math.toIntExact(event.groupId()));
    verify(teamService, never()).updateTeamName(any(), anyString());
  }

  @Test
  void onEnergyPortalOrganisationGroupEvent_whenGroupUpdated_andNameNotChanged_thenDoNothing(){
    createEvent("Test Organisation", false);

    when(teamService.findByEnergyPortalOrgGroupId(anyInt())).thenReturn(Optional.of(TEAM));

    teamUpdateHandler.onEnergyPortalOrganisationGroupEvent(event);

    verify(teamService).findByEnergyPortalOrgGroupId(Math.toIntExact(event.groupId()));
    verify(teamService, never()).updateTeamName(any(), anyString());
  }

  @Test
  void onEnergyPortalOrganisationGroupEvent_whenGroupUpdated_andNameChanged_thenUpdateTeamName(){
    createEvent("Updated Test Organisation", false);

    when(teamService.findByEnergyPortalOrgGroupId(anyInt())).thenReturn(Optional.of(TEAM));

    teamUpdateHandler.onEnergyPortalOrganisationGroupEvent(event);

    verify(teamService).findByEnergyPortalOrgGroupId(Math.toIntExact(event.groupId()));
    verify(teamService).updateTeamName(TEAM, event.name());
  }

  private void createEvent(String name, boolean isCreated){
    event = new EnergyPortalOrganisationGroupEvent(
        1L,
        name,
        "TEST",
        isCreated
    );
  }

}