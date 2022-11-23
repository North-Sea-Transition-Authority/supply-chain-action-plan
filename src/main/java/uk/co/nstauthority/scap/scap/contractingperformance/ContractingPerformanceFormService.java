package uk.co.nstauthority.scap.scap.contractingperformance;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.scap.actualtender.activity.ActualTenderActivity;
import uk.co.nstauthority.scap.util.StreamUtils;

@Service
class ContractingPerformanceFormService {

  private final ContractingPerformanceFormValidator validator;

  @Autowired
  ContractingPerformanceFormService(ContractingPerformanceFormValidator validator) {
    this.validator = validator;
  }

  BindingResult validate(ContractingPerformanceForm form, BindingResult bindingResult, List<ActualTenderActivity> activities) {
    var activityIds = activities.stream()
        .map(ActualTenderActivity::getId)
        .collect(Collectors.toSet());
    validator.validate(form, bindingResult, new ContractingPerformanceFormValidatorHint(activityIds));
    return bindingResult;
  }

  Map<String, String> getScopeTitlesMap(List<ActualTenderActivity> actualTenderActivities) {
    return actualTenderActivities.stream()
        .collect(StreamUtils.toLinkedHashMap(
            activity -> String.valueOf(activity.getId()),
            ActualTenderActivity::getScopeTitle
        ));
  }
}
