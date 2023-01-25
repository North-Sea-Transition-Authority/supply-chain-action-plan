package uk.co.nstauthority.scap.scap.actualtender.activity.delete;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.inOrder;

import java.util.Set;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.servlet.mvc.support.RedirectAttributesModelMap;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerBodyLine;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerType;
import uk.co.nstauthority.scap.fds.notificationbanner.NotificationBannerView;
import uk.co.nstauthority.scap.scap.actualtender.ActualTenderService;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivityService;
import uk.co.nstauthority.scap.scap.actualtender.activity.InvitationToTenderParticipantService;
import uk.co.nstauthority.scap.scap.actualtender.activity.awardedcontract.AwardedContractService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceOverviewService;
import uk.co.nstauthority.scap.scap.contractingperformance.ContractingPerformanceService;

@ExtendWith(MockitoExtension.class)
class DeleteActualTenderActivityServiceTest {

  @Mock
  ActualTenderActivityService actualTenderActivityService;

  @Mock
  InvitationToTenderParticipantService invitationToTenderParticipantService;

  @Mock
  AwardedContractService awardedContractService;

  @Mock
  ContractingPerformanceService contractingPerformanceService;

  @Mock
  ActualTenderService actualTenderService;

  @Mock
  ContractingPerformanceOverviewService contractingPerformanceOverviewService;

  @InjectMocks
  DeleteActualTenderActivityService deleteActualTenderActivityService;

  @Test
  void deleteActualTenderActivity_VerifyCalls() {
    var actualTenderActivity = new ActualTenderActivity(44);

    deleteActualTenderActivityService.deleteActualTenderActivity(actualTenderActivity);

    var inOrder = inOrder(
        contractingPerformanceService,
        awardedContractService,
        invitationToTenderParticipantService,
        actualTenderActivityService
    );

    inOrder.verify(contractingPerformanceService).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(awardedContractService).deleteByActualTenderActivity(actualTenderActivity);
    inOrder.verify(invitationToTenderParticipantService).deleteAllByActualTenderActivity(actualTenderActivity);
    inOrder.verify(actualTenderActivityService).deleteActualTenderActivity(actualTenderActivity);
  }

  @Test
  void addActualTenderDeletionSuccessBanner() {
    var scopeTitle = "Some scope title";
    var expectedBodyContent = "%s has been removed from this SCAP".formatted(scopeTitle);
    var redirectAttributes = new RedirectAttributesModelMap();

    deleteActualTenderActivityService.addActualTenderDeletionSuccessBanner(redirectAttributes, scopeTitle);

    assertThat(redirectAttributes.getFlashAttributes()).hasSize(1);
    var notificationBannerView = (NotificationBannerView) redirectAttributes.getFlashAttributes()
        .get("notificationBannerView");
    assertThat(notificationBannerView).extracting(
        NotificationBannerView::getTitle,
        NotificationBannerView::getBannerType,
        view -> view.getBodyLines().stream()
            .map(NotificationBannerBodyLine::lineText)
            .collect(Collectors.toSet())
    ).containsExactly(
        "Success",
        NotificationBannerType.SUCCESS,
        Set.of(expectedBodyContent)
    );
  }
}
