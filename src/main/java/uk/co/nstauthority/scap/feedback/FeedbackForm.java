package uk.co.nstauthority.scap.feedback;

import uk.co.fivium.formlibrary.input.StringInput;

class FeedbackForm {

  static final String SATISFACTION_FIELD_NAME = "satisfactionRating";

  private SatisfactionRating satisfactionRating;

  private final StringInput comments;

  public FeedbackForm() {
    comments = new StringInput("comments", "comments");
  }

  public SatisfactionRating getSatisfactionRating() {
    return satisfactionRating;
  }

  public void setSatisfactionRating(SatisfactionRating satisfactionRating) {
    this.satisfactionRating = satisfactionRating;
  }

  public StringInput getComments() {
    return comments;
  }

  public void setComments(String comments) {
    this.comments.setInputValue(comments);
  }
}
