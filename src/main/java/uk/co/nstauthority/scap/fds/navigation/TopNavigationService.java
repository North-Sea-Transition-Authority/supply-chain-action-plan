package uk.co.nstauthority.scap.fds.navigation;

import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import java.util.ArrayList;
import java.util.List;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamManagementController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@Service
public class TopNavigationService {

  public static final String TEAM_MANAGEMENT_NAVIGATION_ITEM_TITLE = "Manage teams";
  public static final String WORK_AREA_TITLE = "Work area";

  public List<TopNavigationItem> getTopNavigationItems() {
    var navigationItems = new ArrayList<TopNavigationItem>();
    navigationItems.add(
        new TopNavigationItem(WORK_AREA_TITLE,
            ReverseRouter.route(on(WorkAreaController.class).getWorkArea()))
    );

    navigationItems.add(
        new TopNavigationItem(TEAM_MANAGEMENT_NAVIGATION_ITEM_TITLE,
            ReverseRouter.route(on(TeamManagementController.class).renderTeamList()))
    );

    return navigationItems;
  }
}
