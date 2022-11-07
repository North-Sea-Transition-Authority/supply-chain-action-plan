package uk.co.nstauthority.scap.application.plannedtender.hasplannedtender;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.validation.BindingResult;
import uk.co.nstauthority.scap.application.detail.ScapDetail;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTender;
import uk.co.nstauthority.scap.application.plannedtender.ScapPlannedTenderService;
import uk.co.nstauthority.scap.enumutil.YesNo;

@Service
public class ScapHasPlannedTenderFormService {

  private final ScapHasPlannedTenderFormValidator scapHasPlannedTenderFormValidator;
  private final ScapPlannedTenderService scapPlannedTenderService;

  @Autowired
  public ScapHasPlannedTenderFormService(ScapHasPlannedTenderFormValidator scapHasPlannedTenderFormValidator,
                                         ScapPlannedTenderService scapPlannedTenderService) {
    this.scapHasPlannedTenderFormValidator = scapHasPlannedTenderFormValidator;
    this.scapPlannedTenderService = scapPlannedTenderService;
  }

  public BindingResult validate(ScapHasPlannedTenderForm form, BindingResult bindingResult) {
    scapHasPlannedTenderFormValidator.validate(form, bindingResult);
    return bindingResult;
  }

  public ScapHasPlannedTenderForm getForm(ScapDetail scapDetail) {
    return scapPlannedTenderService.getScapPlannedTenderByScapDetail(scapDetail)
        .map(this::scapPlannedTenderToForm)
        .orElse(new ScapHasPlannedTenderForm());
  }

  private ScapHasPlannedTenderForm scapPlannedTenderToForm(ScapPlannedTender scapPlannedTender) {
    var form = new ScapHasPlannedTenderForm();
    if (Boolean.TRUE.equals(scapPlannedTender.getHasPlannedTenders())) {
      form.setHasPlannedTender(YesNo.YES);
    } else {
      form.setHasPlannedTender(YesNo.NO);
    }
    return form;
  }
}
