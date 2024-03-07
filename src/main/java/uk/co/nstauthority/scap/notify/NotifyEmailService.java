package uk.co.nstauthority.scap.notify;

import javax.transaction.Transactional;

public interface NotifyEmailService {

  /**
   * Method to email a single recipient.
   * @param emailProperties The properties for the mail merge fields in the email template
   * @param toEmailAddress The email address to send the email too
   * @return true if an email has been successfully sent otherwise false
   */
  @Transactional
  boolean sendEmail(EmailProperties emailProperties, String toEmailAddress);

  /**
   * Method to email a single recipient.
   * @param emailProperties The properties for the mail merge fields in the email template
   * @param toEmailAddress The email address to send the email too
   * @param reference Identifies a single unique notification or a batch of notifications
   * @param emailReplyToId Specified email ID to receive replies from the users
   * @return true if an email has been successfully sent otherwise false
   */
  @Transactional
  boolean sendEmail(EmailProperties emailProperties,
                    String toEmailAddress,
                    String reference,
                    String emailReplyToId);
}