package uk.co.nstauthority.scap.feedback;

import java.time.Instant;
import uk.co.fivium.feedbackmanagementservice.client.FeedbackManagementServiceFeedback;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;

public class Feedback implements FeedbackManagementServiceFeedback {

  private final String submitterName;
  private final String submitterEmail;
  private final String serviceRating;
  private final String comment;
  private final Instant timestamp;
  private final Integer transactionId;
  private final String transactionReference;
  private final String transactionLink;

  private Feedback(String submitterName,
                   String submitterEmail,
                   String serviceRating,
                   String comment,
                   Instant timestamp,
                   Integer transactionId,
                   String transactionReference,
                   String transactionLink) {
    this.submitterName = submitterName;
    this.submitterEmail = submitterEmail;
    this.serviceRating = serviceRating;
    this.comment = comment;
    this.timestamp = timestamp;
    this.transactionId = transactionId;
    this.transactionReference = transactionReference;
    this.transactionLink = transactionLink;
  }

  public static Builder builder() {
    return new Builder();
  }

  @Override
  public String getSubmitterName() {
    return submitterName;
  }

  @Override
  public String getSubmitterEmail() {
    return submitterEmail;
  }

  @Override
  public String getServiceRating() {
    return serviceRating;
  }

  @Override
  public String getComment() {
    return comment;
  }

  @Override
  public Instant getGivenDatetime() {
    return timestamp;
  }

  @Override
  public Integer getTransactionId() {
    return transactionId;
  }

  @Override
  public String getTransactionReference() {
    return transactionReference;
  }

  @Override
  public String getTransactionLink() {
    return transactionLink;
  }

  @Override
  public String toString() {
    return "Feedback{" +
        "submitterName='" + submitterName + '\'' +
        ", submitterEmail='" + submitterEmail + '\'' +
        ", serviceRating='" + serviceRating + '\'' +
        ", comment='" + comment + '\'' +
        ", timestamp=" + timestamp +
        ", transactionId=" + transactionId +
        ", transactionReference='" + transactionReference + '\'' +
        ", transactionLink='" + transactionLink + '\'' +
        '}';
  }

  public static class Builder {

    private String submitterName;
    private String submitterEmail;
    private String serviceRating;
    private String comment;
    private Instant timestamp;
    private Integer transactionId;
    private String transactionReference;
    private String transactionLink;

    private Builder() {

    }

    public Builder userDetails(ServiceUserDetail userDetails) {
      this.submitterName = userDetails.displayName();
      this.submitterEmail = userDetails.emailAddress();
      return this;
    }

    public Builder serviceRating(SatisfactionRating satisfactionRating) {
      this.serviceRating = satisfactionRating.toString();
      return this;
    }

    public Builder comment(String comment) {
      this.comment = comment;
      return this;
    }

    public Builder timestamp(Instant timestamp) {
      this.timestamp = timestamp;
      return this;
    }

    public Builder transactionId(Integer transactionId) {
      this.transactionId = transactionId;
      return this;
    }

    public Builder transactionReference(String transactionReference) {
      this.transactionReference = transactionReference;
      return this;
    }

    public Builder transactionLink(String transactionLink) {
      this.transactionLink = transactionLink;
      return this;
    }

    public Feedback build() {
      return new Feedback(
          submitterName,
          submitterEmail,
          serviceRating,
          comment,
          timestamp,
          transactionId,
          transactionReference,
          transactionLink
      );
    }

  }
}
