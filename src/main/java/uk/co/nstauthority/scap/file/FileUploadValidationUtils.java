package uk.co.nstauthority.scap.file;

import java.util.List;
import org.springframework.validation.Errors;
import org.springframework.validation.ValidationUtils;

public class FileUploadValidationUtils {

  private FileUploadValidationUtils() {
    throw new AssertionError();
  }

  public static void rejectIfFileDescriptionsAreEmptyOrWhitespace(Errors errors, List<FileUploadForm> fileUploadForms,
                                                                  String field) {
    int bound = fileUploadForms.size();
    for (int i = 0; i < bound; i++) {
      ValidationUtils.rejectIfEmptyOrWhitespace(errors, "%s[%d].uploadedFileDescription".formatted(field, i),
          "%s[%d].uploadedFileDescription.required".formatted(field, i), "Enter a description of this file");
    }
  }
}
