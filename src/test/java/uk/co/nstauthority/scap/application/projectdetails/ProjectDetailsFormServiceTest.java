package uk.co.nstauthority.scap.application.projectdetails;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.validation.BeanPropertyBindingResult;

@ExtendWith(MockitoExtension.class)
class ProjectDetailsFormServiceTest {

  @Mock
  ProjectDetailsFormValidator projectDetailsFormValidator;

  @InjectMocks
  ProjectDetailsFormService projectDetailsFormService;

  @Test
  void validate_verifyCallsValidator() {
    var form = new ProjectDetailsForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    var returnedBindingResult = projectDetailsFormService.validate(form, bindingResult);

    verify(projectDetailsFormValidator, times(1)).validate(form, bindingResult);
    assertThat(returnedBindingResult).isEqualTo(bindingResult);
  }
}
