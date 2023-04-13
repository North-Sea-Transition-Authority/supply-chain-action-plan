package uk.co.nstauthority.scap.scap.plannedtender.hasplannedtender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.enumutil.YesNo;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTender;
import uk.co.nstauthority.scap.scap.plannedtender.PlannedTenderService;

@Service
public class HasPlannedTenderFormService {

  private final HasPlannedTenderFormValidator hasPlannedTenderFormValidator;
  private final PlannedTenderService plannedTenderService;

  @Autowired
  public HasPlannedTenderFormService(HasPlannedTenderFormValidator hasPlannedTenderFormValidator,
                                     PlannedTenderService plannedTenderService) {
    this.hasPlannedTenderFormValidator = hasPlannedTenderFormValidator;
    this.plannedTenderService = plannedTenderService;
  }

  public BindingResult validate(HasPlannedTenderForm form, BindingResult bindingResult) {
    hasPlannedTenderFormValidator.validate(form, bindingResult);
    return bindingResult;
  }

  public HasPlannedTenderForm getForm(ScapDetail scapDetail) {
    return plannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)
        .map(this::scapPlannedTenderToForm)
        .orElse(new HasPlannedTenderForm());
  }

  public HasPlannedTenderForm scapPlannedTenderToForm(PlannedTender plannedTender) {
    var form = new HasPlannedTenderForm();
    if (Boolean.TRUE.equals(plannedTender.getHasPlannedTenders())) {
      form.setHasPlannedTender(YesNo.YES);
    } else {
      form.setHasPlannedTender(YesNo.NO);
    }
    return form;
  }
}
