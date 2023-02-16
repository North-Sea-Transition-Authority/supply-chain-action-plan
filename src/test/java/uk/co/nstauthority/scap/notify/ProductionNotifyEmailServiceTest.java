package uk.co.nstauthority.scap.notify;


import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.groups.Tuple.tuple;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Map;
import org.apache.commons.validator.routines.EmailValidator;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.notify.library.FiviumNotificationClientApi;
import uk.co.nstauthority.scap.branding.ServiceConfigurationProperties;
import uk.gov.service.notify.NotificationClientException;

@ExtendWith(MockitoExtension.class)
class ProductionNotifyEmailServiceTest {

  private final ServiceConfigurationProperties serviceConfigurationProperties = new ServiceConfigurationProperties("SCAP", "SCAP");
  @Mock
  private FiviumNotificationClientApi fiviumNotificationClientApi;
  @Mock
  private EmailValidator emailValidator;

  private ProductionNotifyEmailService productionNotifyEmailService;

  @BeforeEach
  void setUp() {
    productionNotifyEmailService = new ProductionNotifyEmailService(fiviumNotificationClientApi, serviceConfigurationProperties, emailValidator);
  }

  @Test
  void sendEmail_EmailNotValid_ReturnsEarly() {
    var emailProperties = createEmailProperties();
    var toEmailAddress = "toEmailAddress";
    var reference = "reference";
    var emailReplyToId = "emailReplyToId";

    when(emailValidator.isValid(toEmailAddress)).thenReturn(false);

    var success = productionNotifyEmailService.sendEmail(emailProperties, toEmailAddress, reference, emailReplyToId);

    assertThat(success).isFalse();
  }

  @Test
  void sendEmail_VerifyCalls() throws NotificationClientException {
    var emailProperties = createEmailProperties();
    var toEmailAddress = "toEmailAddress";
    var reference = "reference";
    var emailReplyToId = "emailReplyToId";

    when(emailValidator.isValid(toEmailAddress)).thenReturn(true);

    var success = productionNotifyEmailService.sendEmail(emailProperties, toEmailAddress, reference, emailReplyToId);

    assertThat(success).isTrue();
    assertThat(emailProperties.getEmailPersonalisations().entrySet())
        .extracting(Map.Entry::getKey, Map.Entry::getValue)
        .containsExactly(
            tuple("SERVICE_NAME", serviceConfigurationProperties.name()),
            tuple("TEST_EMAIL", "no")
        );
    verify(fiviumNotificationClientApi).sendEmail(eq(emailProperties.getTemplate().getTemplateName()), eq(toEmailAddress), anyMap(), eq(reference), eq(emailReplyToId));
  }

  private EmailProperties createEmailProperties() {
    return new EmailProperties(NotifyTemplate.EMAIL_DELIVERY_FAILED);
  }
}
