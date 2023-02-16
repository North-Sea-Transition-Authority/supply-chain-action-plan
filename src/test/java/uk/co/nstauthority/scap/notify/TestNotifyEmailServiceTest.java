package uk.co.nstauthority.scap.notify;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.groups.Tuple.tuple;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
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
import uk.co.nstauthority.scap.configuration.EmailConfiguration;
import uk.gov.service.notify.NotificationClientException;

@ExtendWith(MockitoExtension.class)
class TestNotifyEmailServiceTest {
  private final ServiceConfigurationProperties serviceConfigurationProperties = new ServiceConfigurationProperties("SCAP", "SCAP");
  private final String testEmail1 = "one@test.com";
  private final String testEmail2 = "two@test.com";
  private final EmailConfiguration emailConfig = new EmailConfiguration("test", String.format("%s, %s", testEmail1, testEmail2), "email");
  @Mock
  private FiviumNotificationClientApi fiviumNotificationClientApi;
  @Mock
  private EmailValidator emailValidator;

  private TestNotifyEmailService testNotifyEmailService;

  @BeforeEach
  void setUp() {
    testNotifyEmailService = new TestNotifyEmailService(fiviumNotificationClientApi, serviceConfigurationProperties, emailValidator,
        emailConfig);
  }

  @Test
  void sendEmail_EmailNotValid_ReturnsEarly() {
    var emailProperties = createEmailProperties();
    var toEmailAddress = "toEmailAddress";
    var reference = "reference";
    var emailReplyToId = "emailReplyToId";

    when(emailValidator.isValid(anyString())).thenReturn(false);

    var success = testNotifyEmailService.sendEmail(emailProperties, toEmailAddress, reference, emailReplyToId);

    assertThat(success).isFalse();
  }

  @Test
  void sendEmail_VerifyCalls() throws NotificationClientException {
    var emailProperties = createEmailProperties();
    var toEmailAddress = "toEmailAddress";
    var reference = "reference";
    var emailReplyToId = "emailReplyToId";

    when(emailValidator.isValid(anyString())).thenReturn(true);

    var success = testNotifyEmailService.sendEmail(emailProperties, toEmailAddress, reference, emailReplyToId);

    assertThat(success).isTrue();
    assertThat(emailProperties.getEmailPersonalisations().entrySet())
        .extracting(Map.Entry::getKey, Map.Entry::getValue)
        .containsExactly(
            tuple("SERVICE_NAME", serviceConfigurationProperties.name()),
            tuple("TEST_EMAIL", "yes")
        );
    verify(fiviumNotificationClientApi).sendEmail(eq(emailProperties.getTemplate().getTemplateName()), eq(testEmail1), anyMap(), eq(reference), eq(emailReplyToId));
    verify(fiviumNotificationClientApi).sendEmail(eq(emailProperties.getTemplate().getTemplateName()), eq(testEmail2), anyMap(), eq(reference), eq(emailReplyToId));
  }

  private EmailProperties createEmailProperties() {
    return new EmailProperties(NotifyTemplate.EMAIL_DELIVERY_FAILED);
  }
}