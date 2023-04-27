package uk.co.nstauthority.scap.scap.organisationgroup;

import javax.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.scap.detail.ScapDetail;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Service
class ScapOperatorService {

  private final ScapService scapService;
  private final ScapDetailService scapDetailService;

  @Autowired
  ScapOperatorService(ScapService scapService,
                      ScapDetailService scapDetailService) {
    this.scapService = scapService;
    this.scapDetailService = scapDetailService;
  }

  @Transactional
  public Scap createScap(OrganisationGroupForm form) {
    var scap = scapService.createScap(form.getOrganisationGroupId());
    var scapDetail = scapDetailService.createDraftScapDetail(scap);
    updateScapOperator(scap, scapDetail, form);
    return scap;
  }

  @Transactional
  public void updateScapOperator(Scap scap, ScapDetail scapDetail, OrganisationGroupForm form) {
    scapService.updateScapOrganisationGroup(scap, form.getOrganisationGroupId());
    var parentScap = getParentScap(form);
    scapDetailService.setTierOneContractor(scapDetail, parentScap, form);
  }

  private Scap getParentScap(OrganisationGroupForm form) {
    if (Boolean.TRUE.equals(form.getIsTierOneContractor())) {
      return scapService.getScapById(form.getParentScapId());
    }
    return null;
  }
}
