package uk.co.nstauthority.scap.energyportal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportal.starter.usercontext.VersionedUserContext;
import uk.co.nstauthority.scap.TestEntityProvider;
import uk.co.nstauthority.scap.energyportal.usercontext.ScapEnergyPortalUserServicesContextProvider;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.utils.EnergyPortalUserDtoTestUtil;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequest;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;

@ExtendWith(MockitoExtension.class)
class ScapEnergyPortalUserServicesContextProviderTest {

  @Mock
  private EnergyPortalUserService energyPortalUserService;

  @Mock
  private TeamService teamService;

  @Mock
  private ScapService scapService;

  @Mock
  private UpdateRequestService updateRequestService;

  private ScapEnergyPortalUserServicesContextProvider provider;

  private static final Clock CLOCK = Clock.fixed(Instant.now(), ZoneId.of("UTC"));
  private static final LocalDate TODAY = LocalDate.now(CLOCK);
  private final long wuaId = 123L;

  @BeforeEach
  void setUp() {
    provider = new ScapEnergyPortalUserServicesContextProvider(
        energyPortalUserService, teamService, scapService, updateRequestService, CLOCK
    );

    var user = EnergyPortalUserDtoTestUtil.Builder().build();
    when(energyPortalUserService.getEnergyPortalUser(any())).thenReturn(user);

    when(teamService.getTeamsOfTypeThatUserBelongsTo(any(), eq(TeamType.INDUSTRY)))
        .thenReturn(List.of(mock(Team.class)));

    when(scapService.getAllScapsForTeams(any()))
        .thenReturn(List.of(TestEntityProvider.getScap()));
  }

  @Test
  void returnEmptyContext_whenNoRequests() {
    when(updateRequestService.findUnresolvedRequestsForScaps(any())).thenReturn(Collections.emptyList());

    var expectedContext = VersionedUserContext.newBuilder().v1().build();

    assertThat(provider.getUserContext(wuaId)).isEqualTo(expectedContext);
  }

  @Test
  void highContextShown_whenOneOverdueRequest() {
    var req = new UpdateRequest(UUID.randomUUID());
    req.setDueDate(TODAY.minusDays(1));

    when(updateRequestService.findUnresolvedRequestsForScaps(any())).thenReturn(List.of(req));

    var expectedContext = VersionedUserContext.newBuilder().v1()
        .high(1, "update overdue")
        .build();

    assertThat(provider.getUserContext(wuaId)).isEqualTo(expectedContext);
  }

  @Test
  void highContextShown_whenMultipleOverdueRequests() {
    var req1 = new UpdateRequest(UUID.randomUUID());
    req1.setDueDate(TODAY.minusDays(2));

    var req2 = new UpdateRequest(UUID.randomUUID());
    req2.setDueDate(TODAY.minusDays(5));

    when(updateRequestService.findUnresolvedRequestsForScaps(any())).thenReturn(List.of(req1, req2));

    var expectedContext = VersionedUserContext.newBuilder().v1()
        .high(2, "updates overdue")
        .build();

    assertThat(provider.getUserContext(wuaId)).isEqualTo(expectedContext);
  }

  @Test
  void lowContext_whenOneOnTimeRequest() {
    var req = new UpdateRequest(UUID.randomUUID());
    req.setDueDate(TODAY.plusDays(1));

    when(updateRequestService.findUnresolvedRequestsForScaps(any())).thenReturn(List.of(req));

    var expectedContext = VersionedUserContext.newBuilder().v1()
        .low(1, "update requested")
        .build();

    assertThat(provider.getUserContext(wuaId)).isEqualTo(expectedContext);
  }

  @Test
  void lowContextShown_whenOnTime() {
    var req1 = new UpdateRequest(UUID.randomUUID());
    req1.setDueDate(TODAY.plusDays(5));

    when(updateRequestService.findUnresolvedRequestsForScaps(any())).thenReturn(List.of(req1));

    var expectedContext = VersionedUserContext.newBuilder().v1()
        .low(1, "update requested")
        .build();

    assertThat(provider.getUserContext(wuaId)).isEqualTo(expectedContext);
  }

  @Test
  void bothContextsShown_whenMixedRequestsExist() {
    var overdueReq = new UpdateRequest(UUID.randomUUID());
    overdueReq.setDueDate(TODAY.minusDays(1));

    var onTimeReq1 = new UpdateRequest(UUID.randomUUID());
    onTimeReq1.setDueDate(TODAY);

    var onTimeReq2 = new UpdateRequest(UUID.randomUUID());
    onTimeReq2.setDueDate(TODAY.plusDays(1));

    when(updateRequestService.findUnresolvedRequestsForScaps(any()))
        .thenReturn(List.of(overdueReq, onTimeReq1, onTimeReq2));

    var expectedContext = VersionedUserContext.newBuilder().v1()
        .high(1, "update overdue")
        .low(2, "updates requested")
        .build();

    assertThat(provider.getUserContext(wuaId)).isEqualTo(expectedContext);
  }
}