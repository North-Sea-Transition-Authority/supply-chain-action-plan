package uk.co.nstauthority.scap.notify;

import java.util.HashMap;
import java.util.Map;

public class EmailProperties {

  private final NotifyTemplate template;
  private final Map<String, String> emailPersonalisations;

  public EmailProperties(NotifyTemplate template) {
    this.template = template;

    emailPersonalisations = new HashMap<>();
    // TEST_EMAIL set to "no" by default
    emailPersonalisations.put("TEST_EMAIL", "no");
  }

  public NotifyTemplate getTemplate() {
    return template;
  }

  public Map<String, String> getEmailPersonalisations() {
    return emailPersonalisations;
  }
}
