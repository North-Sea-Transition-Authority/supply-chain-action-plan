package uk.co.nstauthority.scap.enumutil;

/**
 * Enums that implement this interface might be passed as an option to a list of answers in a form.
 * For example a list of answers in a radio group.
 */
public interface DisplayableEnumOption extends Displayable {

  int getDisplayOrder();

  String getFormValue();

  String getDescription();
}
