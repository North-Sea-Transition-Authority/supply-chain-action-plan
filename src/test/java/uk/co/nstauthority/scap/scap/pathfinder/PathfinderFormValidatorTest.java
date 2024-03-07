package uk.co.nstauthority.scap.scap.pathfinder;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.entry;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import uk.co.fivium.energyportalapi.generated.types.PathfinderProject;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.utils.ValidatorTestingUtil;

@ExtendWith(MockitoExtension.class)
class PathfinderFormValidatorTest {

  @Mock
  private PathfinderProjectService pathfinderProjectService;

  @InjectMocks
  PathfinderFormValidator pathfinderFormValidator;

  private PathfinderForm form;

  @BeforeEach
  void setup() {
    form = new PathfinderForm();
  }

  @Test
  void supports_NonSupportedClass_AssertFalse() {
    assertFalse(pathfinderFormValidator.supports(ValidatorTestingUtil.NonSupportedClass.class));
  }

  @Test
  void supports_PathfinderForm_AssertTrue() {
    assertTrue(pathfinderFormValidator.supports(PathfinderForm.class));
  }

  @Test
  void validate_EmptyForm_AssertError() {
    var bindingResult = pathfinderFormValidator.validate(form);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(PathfinderForm.HAS_PATHFINDER_PROJECTS_FIELD, Collections.singleton("required"))
    );
  }

  @Test
  void validate_NoPathfinderProjects_MissingRationale_AssertError() {
    form.setHasPathfinderProjects(false);

    var bindingResult = pathfinderFormValidator.validate(form);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(
            PathfinderForm.NO_PATHFINDER_PROJECT_RATIONALE_FIELD + ".inputValue",
            Collections.singleton(PathfinderForm.NO_PATHFINDER_PROJECT_RATIONALE_FIELD + ".required")
        )
    );
  }

  @Test
  void validate_NoPathfinderProjects_ValidForm() {
    form.setHasPathfinderProjects(false);
    form.setNoPathfinderProjectRationale("Some rationale");

    var bindingResult = pathfinderFormValidator.validate(form);

    assertFalse(bindingResult.hasErrors());
  }

  @ParameterizedTest
  @NullAndEmptySource
  void validate_HasPathfinderProjects_NoneSelected_AssertError(List<Integer> pathfinderProjectIds) {
    form.setHasPathfinderProjects(true);
    form.setPathfinderProjectIds(pathfinderProjectIds);

    var bindingResult = pathfinderFormValidator.validate(form);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(PathfinderForm.PATHFINDER_PROJECTS_SELECTOR_FIELD, Collections.singleton("required"))
    );
  }

  @Test
  void validate_HasPathfinderProjects_InvalidProjects_AssertError() {
    var projectIds = Collections.singletonList(12);
    form.setHasPathfinderProjects(true);
    form.setPathfinderProjectIds(projectIds);

    when(pathfinderProjectService.getPathfinderProjectsByIds(projectIds))
        .thenReturn(Collections.emptyList());

    var bindingResult = pathfinderFormValidator.validate(form);
    var extractedErrors = ValidatorTestingUtil.extractErrors(bindingResult);

    assertThat(extractedErrors).containsExactly(
        entry(PathfinderForm.PATHFINDER_PROJECTS_SELECTOR_FIELD, Collections.singleton("invalid"))
    );
  }

  @Test
  void validate_HasPathfinderProjects_ValidForm() {
    var projectIds = Collections.singletonList(12);
    form.setHasPathfinderProjects(true);
    form.setPathfinderProjectIds(projectIds);
    var pathfinderProject = PathfinderProject.newBuilder().build();

    when(pathfinderProjectService.getPathfinderProjectsByIds(projectIds))
        .thenReturn(Collections.singletonList(pathfinderProject));

    var bindingResult = pathfinderFormValidator.validate(form);

    assertFalse(bindingResult.hasErrors());
  }
}
