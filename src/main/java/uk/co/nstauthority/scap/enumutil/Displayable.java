package uk.co.nstauthority.scap.enumutil;

import uk.co.fivium.digitalenummaterialisationlibrary.enummaterialisation.MaterialisableEnum;

/**
 * Interface which allows implementing classes to expose their human readable representation consistently.
 */
public interface Displayable extends MaterialisableEnum {

  String getDisplayName();

  String getEnumName();
}
