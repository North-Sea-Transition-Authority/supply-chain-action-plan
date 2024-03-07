package uk.co.nstauthority.scap.scap.organisationgroup;

public class OrganisationGroupForm {

  private Integer organisationGroupId;
  private Boolean isTierOneContractor;
  private Integer parentScapId;

  public Integer getOrganisationGroupId() {
    return organisationGroupId;
  }

  public void setOrganisationGroupId(Integer organisationGroupId) {
    this.organisationGroupId = organisationGroupId;
  }

  public Boolean getIsTierOneContractor() {
    return isTierOneContractor;
  }

  public void setIsTierOneContractor(Boolean tierOneContractor) {
    isTierOneContractor = tierOneContractor;
  }

  public Integer getParentScapId() {
    return parentScapId;
  }

  public void setParentScapId(Integer parentScapId) {
    this.parentScapId = parentScapId;
  }
}
