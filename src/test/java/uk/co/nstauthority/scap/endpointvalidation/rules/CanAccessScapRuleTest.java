package uk.co.nstauthority.scap.endpointvalidation.rules;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import uk.co.nstauthority.scap.endpointvalidation.annotations.CanAccessScap;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.permissionmanagement.industry.IndustryTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberRole;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamMemberService;
import uk.co.nstauthority.scap.permissionmanagement.teams.TeamService;
import uk.co.nstauthority.scap.scap.detail.ScapDetailStatus;

@ExtendWith(MockitoExtension.class)
class CanAccessScapRuleTest extends AbstractInterceptorRuleTest {

  @Mock
  TeamService teamService;

  @Mock
  TeamMemberService teamMemberService;

  @InjectMocks
  CanAccessScapRule rule;

  @Test
  void whenDraftAndIndustryUserHasSubmitPermission_thenOk() {
    scap.setOrganisationGroupId(1);
    scapDetail.setStatus(ScapDetailStatus.DRAFT);
    var industryTeam = new Team();
    industryTeam.setTeamType(TeamType.INDUSTRY);
    var regulatorTeam = new Team();
    regulatorTeam.setTeamType(TeamType.REGULATOR);

    when(teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);

    when(teamService.getRegulatorTeam())
        .thenReturn(regulatorTeam);

    var teamMemberRole = new TeamMemberRole(UUID.randomUUID());
    teamMemberRole.setRole(IndustryTeamRole.SCAP_SUBMITTER.getEnumName());
    teamMemberRole.setTeam(industryTeam);
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    var annotation = getAnnotation(
        TestController.class,
        CanAccessScap.class
    );

    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scapDetail,
        null
    );

    assertTrue(interceptorResult.hasRulePassed());
    verifyNoInteractions(response);
  }

  @ParameterizedTest
  @EnumSource(
      value = ScapDetailStatus.class,
      names = {"DRAFT", "DELETED"},
      mode = EnumSource.Mode.EXCLUDE)
  void whenPostSubmissionAndIndustryUserHasAnyViewPermissions_thenOk(ScapDetailStatus status) {
    scap.setOrganisationGroupId(1);
    scapDetail.setStatus(status);
    var industryTeam = new Team();
    industryTeam.setTeamType(TeamType.INDUSTRY);
    var regulatorTeam = new Team();
    regulatorTeam.setTeamType(TeamType.REGULATOR);

    when(teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);

    when(teamService.getRegulatorTeam())
        .thenReturn(regulatorTeam);

    var teamMemberRole = new TeamMemberRole(UUID.randomUUID());
    teamMemberRole.setRole(IndustryTeamRole.SCAP_SUBMITTER.getEnumName());
    teamMemberRole.setTeam(industryTeam);
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    var annotation = getAnnotation(
        TestController.class,
        CanAccessScap.class
    );

    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scapDetail,
        null
    );

    assertTrue(interceptorResult.hasRulePassed());
    verifyNoInteractions(response);
  }

  @ParameterizedTest
  @EnumSource(
      value = ScapDetailStatus.class,
      names = {"DRAFT", "DELETED"},
      mode = EnumSource.Mode.EXCLUDE)
  void whenPostSubmissionAndRegulatorUserHasAnyViewPermissions_thenOk(ScapDetailStatus status) {
    scap.setOrganisationGroupId(1);
    scapDetail.setStatus(status);
    var industryTeam = new Team();
    industryTeam.setTeamType(TeamType.INDUSTRY);
    var regulatorTeam = new Team();
    regulatorTeam.setTeamType(TeamType.REGULATOR);

    when(teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);

    when(teamService.getRegulatorTeam())
        .thenReturn(regulatorTeam);

    var teamMemberRole = new TeamMemberRole(UUID.randomUUID());
    teamMemberRole.setRole(RegulatorTeamRole.SCAP_VIEWER.getEnumName());
    teamMemberRole.setTeam(regulatorTeam);
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    var annotation = getAnnotation(
        TestController.class,
        CanAccessScap.class
    );

    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scapDetail,
        null
    );

    assertTrue(interceptorResult.hasRulePassed());
    verifyNoInteractions(response);
  }

  @ParameterizedTest
  @EnumSource(
      value = ScapDetailStatus.class,
      names = {"DRAFT", "DELETED"},
      mode = EnumSource.Mode.EXCLUDE)
  void whenPostSubmissionAndNoRegulatorSubmitPermission_thenForbidden(ScapDetailStatus status) {
    scap.setOrganisationGroupId(1);
    scapDetail.setStatus(status);
    var industryTeam = new Team();
    industryTeam.setTeamType(TeamType.INDUSTRY);
    var regulatorTeam = new Team();
    regulatorTeam.setTeamType(TeamType.REGULATOR);

    when(teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);

    when(teamService.getRegulatorTeam())
        .thenReturn(regulatorTeam);

    var teamMemberRole = new TeamMemberRole(UUID.randomUUID());
    teamMemberRole.setRole(RegulatorTeamRole.ACCESS_MANAGER.getEnumName());
    teamMemberRole.setTeam(regulatorTeam);
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    var annotation = getAnnotation(
        TestController.class,
        CanAccessScap.class
    );

    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scapDetail,
        null
    );

    assertFalse(interceptorResult.hasRulePassed());
    assertThat(interceptorResult.failureStatus()).isEqualTo(HttpStatus.FORBIDDEN);
  }

  @ParameterizedTest
  @EnumSource(
      value = ScapDetailStatus.class,
      names = {"DRAFT", "DELETED"},
      mode = EnumSource.Mode.EXCLUDE)
  void whenPostSubmissionAndNoIndustrySubmitPermission_thenForbidden(ScapDetailStatus status) {
    scap.setOrganisationGroupId(1);
    scapDetail.setStatus(status);
    var industryTeam = new Team();
    industryTeam.setTeamType(TeamType.INDUSTRY);
    var regulatorTeam = new Team();
    regulatorTeam.setTeamType(TeamType.REGULATOR);

    when(teamService.getByEnergyPortalOrgGroupId(scapDetail.getScap().getOrganisationGroupId()))
        .thenReturn(industryTeam);

    when(teamService.getRegulatorTeam())
        .thenReturn(regulatorTeam);

    var teamMemberRole = new TeamMemberRole(UUID.randomUUID());
    teamMemberRole.setRole(IndustryTeamRole.ACCESS_MANAGER.getEnumName());
    teamMemberRole.setTeam(industryTeam);
    when(teamMemberService.findAllRolesByUser(List.of(industryTeam, regulatorTeam), userDetail.getWebUserAccountId()))
        .thenReturn(List.of(teamMemberRole));

    var annotation = getAnnotation(
        TestController.class,
        CanAccessScap.class
    );

    var interceptorResult = rule.check(
        annotation,
        request,
        response,
        userDetail,
        scapDetail,
        null
    );

    assertFalse(interceptorResult.hasRulePassed());
    assertThat(interceptorResult.failureStatus()).isEqualTo(HttpStatus.FORBIDDEN);
  }

  @Controller
  @RequestMapping("/route1")
  @CanAccessScap
  private static class TestController {
    private static final String ENDPOINT_DATA = "Hello world!";
    @GetMapping("/route2")
    String getWithClassAnnotation() {
      return ENDPOINT_DATA;
    }
  }
}