package uk.co.nstauthority.scap.energyportal.user;

import uk.co.nstauthority.scap.permissionmanagement.Team;

public interface AllowedDomainService {

  boolean isAllowedDomain(String domain, Team team);
}