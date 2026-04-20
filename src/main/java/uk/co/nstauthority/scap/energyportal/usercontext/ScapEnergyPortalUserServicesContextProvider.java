package uk.co.nstauthority.scap.energyportal.usercontext;

import java.time.Clock;
import java.time.LocalDate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import uk.co.fivium.energyportal.starter.usercontext.EnergyPortalUserServicesContextProvider;
import uk.co.fivium.energyportal.starter.usercontext.VersionedUserContext;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.workarea.updaterequests.UpdateRequestService;

@Component
public class ScapEnergyPortalUserServicesContextProvider implements EnergyPortalUserServicesContextProvider {

  private final EnergyPortalUserService energyPortalUserService;
  private final TeamService teamService;
  private final ScapService scapService;
  private final UpdateRequestService updateRequestService;
  private final Clock clock;

  @Autowired
  public ScapEnergyPortalUserServicesContextProvider(EnergyPortalUserService energyPortalUserService,
                                                     TeamService teamService,
                                                     ScapService scapService,
                                                     UpdateRequestService updateRequestService,
                                                     Clock clock) {
    this.energyPortalUserService = energyPortalUserService;
    this.teamService = teamService;
    this.scapService = scapService;
    this.updateRequestService = updateRequestService;
    this.clock = clock;
  }

  @Override
  public VersionedUserContext getUserContext(long wuaId) {
    var contextBuilder = VersionedUserContext.newBuilder().v1();
    var webUserAccountId = new WebUserAccountId(wuaId);

    var userDetail = ServiceUserDetail.from(energyPortalUserService.getEnergyPortalUser(webUserAccountId));
    var teams = teamService.getTeamsOfTypeThatUserBelongsTo(userDetail, TeamType.INDUSTRY);
    var scaps = scapService.getAllScapsForTeams(teams);
    var outstandingRequests = updateRequestService.findUnresolvedRequestsForScaps(scaps);
    var now = LocalDate.now(clock);

    var overdueCount = Math.toIntExact(outstandingRequests.stream()
        .filter(req -> req.getDueDate() != null && req.getDueDate().isBefore(now))
        .count());
    var onTimeCount = outstandingRequests.size() - overdueCount;

    if (overdueCount > 0) {
      contextBuilder.high(
          overdueCount,
          "update%s overdue".formatted(overdueCount == 1 ? "" : "s")
      );
    }

    if (onTimeCount > 0) {
      contextBuilder.low(
          onTimeCount,
          "update%s requested".formatted(onTimeCount == 1 ? "" : "s")
      );
    }
    return contextBuilder.build();
  }
}