package uk.co.nstauthority.scap.fds.topnavigation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.apache.commons.lang3.StringUtils;
import org.assertj.core.groups.Tuple;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationItem;
import uk.co.nstauthority.scap.fds.navigation.TopNavigationService;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamManagementController;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamManagementController;
import uk.co.nstauthority.scap.workarea.WorkAreaController;

@ExtendWith(MockitoExtension.class)
class TopNavigationServiceTest {

  @InjectMocks
  private TopNavigationService topNavigationService;

  @Test
  void getTopNavigationItems_verifyAllTopNavigationItems() {
    var topNavigationItems = topNavigationService.getTopNavigationItems();

    assertThat(topNavigationItems)
        .extracting(
            TopNavigationItem::getDisplayName,
            TopNavigationItem::getUrl
        )
        .containsExactly(
            Tuple.tuple(
                TopNavigationService.WORK_AREA_TITLE,
                StringUtils.stripEnd(ReverseRouter.route(on(WorkAreaController.class).getWorkArea()), "/")
            ),
            Tuple.tuple(
                TopNavigationService.TEAM_MANAGEMENT_NAVIGATION_ITEM_TITLE,
                StringUtils.stripEnd(
                    ReverseRouter.route(on(TeamManagementController.class).renderTeamList()), "/")
            )
        );
  }
}