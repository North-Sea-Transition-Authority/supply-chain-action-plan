package uk.co.nstauthority.scap.restapi.scap;

import java.util.function.Function;
import java.util.stream.Collectors;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchItem;
import uk.co.nstauthority.scap.fds.searchselector.RestSearchResult;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;

@Service
public class ScapRestService {

  static final String ORG_GROUP_REQUEST_PURPOSE = "Get org group names of existing SCAPs";

  private final ScapService scapService;
  private final OrganisationGroupService organisationGroupService;

  @Autowired
  ScapRestService(ScapService scapService,
                  OrganisationGroupService organisationGroupService) {
    this.scapService = scapService;
    this.organisationGroupService = organisationGroupService;
  }

  RestSearchResult searchScaps(String searchTerm) {
    var scaps = scapService.searchByReference(searchTerm);
    var scapOrgGroups = scaps.stream()
        .map(Scap::getOrganisationGroupId)
        .toList();
    var organisationGroups = organisationGroupService
        .getOrganisationGroupsByIds(scapOrgGroups, ORG_GROUP_REQUEST_PURPOSE);
    var scapOrgGroupMap = organisationGroups.stream()
        .collect(Collectors.toMap(
            OrganisationGroup::getOrganisationGroupId,
            Function.identity()
        ));
    var restSearchItems = scaps.stream()
        .map(scap -> ScapRestService.formatSearchResult(scap, scapOrgGroupMap.get(scap.getOrganisationGroupId())))
        .toList();
    return new RestSearchResult(restSearchItems);
  }

  public static RestSearchItem formatSearchResult(Scap scap, OrganisationGroup organisationGroup) {
    return new RestSearchItem(
        String.valueOf(scap.getId()),
        "%s - %s".formatted(scap.getReference(), organisationGroup.getName())
    );
  }
}
