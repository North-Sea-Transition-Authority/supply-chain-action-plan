package uk.co.nstauthority.scap.scap.pathfinder;

import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.Objects;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import uk.co.fivium.formlibrary.validator.string.StringInputValidator;
import uk.co.nstauthority.scap.energyportal.PathfinderProjectService;
import uk.co.nstauthority.scap.util.ValidatorErrorCodes;

@Service
public class PathfinderFormValidator implements Validator {

  private final PathfinderProjectService pathfinderProjectService;

  @Autowired
  public PathfinderFormValidator(PathfinderProjectService pathfinderProjectService) {
    this.pathfinderProjectService = pathfinderProjectService;
  }

  @Override
  public boolean supports(@NotNull Class<?> clazz) {
    return PathfinderForm.class.equals(clazz);
  }

  public BindingResult validate(PathfinderForm form) {
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    validate(form, bindingResult);
    return bindingResult;
  }

  @Override
  public void validate(@NotNull Object target,
                       @NotNull Errors errors) {
    var form = (PathfinderForm) target;

    validateHasPathfinderProjects(form.getHasPathfinderProjects(), errors);

    if (errors.hasFieldErrors(PathfinderForm.HAS_PATHFINDER_PROJECTS_FIELD)) {
      return;
    }

    if (Boolean.TRUE.equals(form.getHasPathfinderProjects())) {
      validatePathfinderProjects(form.getPathfinderProjectIds(), errors);
    } else {
      StringInputValidator.builder().validate(form.getNoPathfinderProjectRationale(), errors);
    }
  }

  private void validateHasPathfinderProjects(Boolean hasPathfinderProjects, Errors errors) {
    if (Objects.isNull(hasPathfinderProjects)) {
      errors.rejectValue(
          PathfinderForm.HAS_PATHFINDER_PROJECTS_FIELD,
          ValidatorErrorCodes.REQUIRED.getErrorCode(),
          "Select whether there are any related Pathfinder projects"
      );
    }
  }

  private void validatePathfinderProjects(List<Integer> pathfinderProjectIds,
                                          Errors errors) {
    var fieldName = PathfinderForm.PATHFINDER_PROJECTS_SELECTOR_FIELD;

    if (CollectionUtils.isEmpty(pathfinderProjectIds)) {
      errors.rejectValue(
          fieldName,
          ValidatorErrorCodes.REQUIRED.getErrorCode(),
          "Select at least one related Pathfinder project"
      );
      return;
    }

    var returnedProjectIds = pathfinderProjectService.getPathfinderProjectsByIds(pathfinderProjectIds);
    if (returnedProjectIds.size() != pathfinderProjectIds.size()) {
      errors.rejectValue(
          PathfinderForm.PATHFINDER_PROJECTS_SELECTOR_FIELD,
          ValidatorErrorCodes.INVALID.getErrorCode()
      );
    }
  }
}
