package uk.co.nstauthority.scap.permissionmanagement;


import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import uk.co.nstauthority.scap.energyportal.WebUserAccountId;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorEditMemberController;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorRemoveMemberController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamView;

public record TeamMemberView(WebUserAccountId wuaId, TeamView teamView, String title, String firstName,
                             String lastName, String contactEmail, String contactNumber,
                             Set<TeamRole> teamRoles) {

  public String getDisplayName() {
    // Title can potentially be null, and so a stream is used to conditionally include it if present.
    return Stream.of(Optional.ofNullable(title), Optional.of(firstName), Optional.of(lastName))
        .filter(Optional::isPresent)
        .map(Optional::get)
        .collect(Collectors.joining(" "));
  }

  public String removeUrl() {
    return switch (teamView.teamType()) {
      case REGULATOR -> ReverseRouter.route(on(RegulatorRemoveMemberController.class)
          .renderRemoveMember(teamView.teamId(), wuaId));
      //TODO: SCAP2022-116 Add Industry Remove Team Member Functionality
      case INDUSTRY -> ReverseRouter.route(on(RegulatorRemoveMemberController.class)
          .renderRemoveMember(teamView.teamId(), wuaId));
    };
  }

  public String editUrl() {
    return switch (teamView.teamType()) {
      case REGULATOR -> ReverseRouter.route(on(RegulatorEditMemberController.class)
          .renderEditMember(teamView.teamId(), wuaId));
      //TODO: SCAP2022-116 Add Industry Edit Member Role functionality
      case INDUSTRY -> ReverseRouter.route(on(RegulatorEditMemberController.class)
          .renderEditMember(teamView.teamId(), wuaId));
    };
  }

}
