package uk.co.nstauthority.scap;

import jakarta.persistence.EntityManager;
import jakarta.transaction.Transactional;
import java.util.Random;
import java.util.Set;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import uk.co.nstauthority.scap.energyportal.EnergyPortalUserService;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRoleService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.organisationgroup.OrganisationGroupService;

@Service
@Profile("development")
class DataBootstrapper {

  private final EntityManager entityManager;

  private static final Logger LOGGER = LoggerFactory.getLogger(DataBootstrapper.class);
  private final EnergyPortalUserService energyPortalUserService;
  private final TeamMemberRoleService teamMemberRoleService;
  private final OrganisationGroupService organisationGroupService;
  private final TeamService teamService;

  public DataBootstrapper(EntityManager entityManager,
                          EnergyPortalUserService energyPortalUserService, TeamMemberRoleService teamMemberRoleService,
                          OrganisationGroupService organisationGroupService, TeamService teamService) {
    this.entityManager = entityManager;
    this.energyPortalUserService = energyPortalUserService;
    this.teamMemberRoleService = teamMemberRoleService;
    this.organisationGroupService = organisationGroupService;
    this.teamService = teamService;
  }

  @EventListener(ApplicationReadyEvent.class)
  @Transactional
  public void loadData() {
    Integer count = (Integer) entityManager.createNativeQuery(
            "SELECT COUNT(*) FROM scap.scaps", Integer.class).getSingleResult();

    if (count > 0) {
      return;
    }

    LOGGER.info("Bootstrapping teams and users");

    var industrySubmitter = energyPortalUserService.findUsersByUsername("industryScapSubmitter@scap.co.uk").getFirst();
    var regulatorCaseOfficer = energyPortalUserService.findUsersByUsername("RegulatorScapCaseOfficer@scap.co.uk").getFirst();
    var industryTeam = new Team();
    industryTeam.setDisplayName("CENTRICA");
    industryTeam.setTeamType(TeamType.INDUSTRY);
    var orgId = organisationGroupService.getOrganisationGroupsByName("CENTRICA", "").getFirst().getOrganisationGroupId();
    industryTeam.setEnergyPortalOrgGroupId(orgId);

    var regulatorTeam = teamService.getRegulatorTeam();

    entityManager.persist(industryTeam);

    teamMemberRoleService.addUserTeamRoles(
        industryTeam,
        industrySubmitter.webUserAccountId(),
        Set.of(IndustryTeamRole.SCAP_SUBMITTER.name())
    );
    teamMemberRoleService.addUserTeamRoles(
        regulatorTeam,
        regulatorCaseOfficer.webUserAccountId(),
        Set.of(RegulatorTeamRole.SCAP_CASE_OFFICER.name(), RegulatorTeamRole.ACCESS_MANAGER.name(),
            RegulatorTeamRole.ORGANISATION_ACCESS_MANAGER.name())
    );
    LOGGER.info("New team {} + {} has been created and added users:", industryTeam.getDisplayName(), industryTeam.getUuid());
    LOGGER.info("{} + {}", industrySubmitter.displayName(), industrySubmitter.webUserAccountId());
  }
}
