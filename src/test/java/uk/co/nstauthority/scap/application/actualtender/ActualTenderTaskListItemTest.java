package uk.co.nstauthority.scap.application.actualtender;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.nstauthority.scap.application.actualtender.hasactualtender.HasActualTenderController;
import uk.co.nstauthority.scap.mvc.ReverseRouter;

@ExtendWith(MockitoExtension.class)
class ActualTenderTaskListItemTest {

  @InjectMocks
  ActualTenderTaskListItem actualTenderTaskListItem;

  private Integer scapId;

  @BeforeEach
  void setup() {
    scapId = 42;
  }

  @Test
  void getActionUrl_assertExpectedUrl() {
    var expectedUrl = ReverseRouter.route(on(HasActualTenderController.class).renderHasActualTenderForm(scapId));

    var returnedUrl = actualTenderTaskListItem.getActionUrl(42);

    assertThat(returnedUrl).isEqualTo(expectedUrl);
  }

  @Test
  void isValid_assertFalse() {
    assertFalse(actualTenderTaskListItem.isValid(scapId));
  }
}
