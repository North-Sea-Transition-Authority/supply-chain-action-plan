package uk.co.nstauthority.scap.scap.organisationgroup;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.security.test.web.servlet.request.SecurityMockMvcRequestPostProcessors.csrf;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;
import static org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder.on;
import static uk.co.nstauthority.scap.mvc.ReverseRouter.emptyBindingResult;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.validation.BeanPropertyBindingResult;
import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import uk.co.fivium.energyportalapi.generated.types.OrganisationGroup;
import uk.co.nstauthority.scap.AbstractScapSubmitterControllerTest;
import uk.co.nstauthority.scap.authentication.ServiceUserDetail;
import uk.co.nstauthority.scap.authentication.TestUserProvider;
import uk.co.nstauthority.scap.fds.ErrorItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.permissionmanagement.Team;
import uk.co.nstauthority.scap.permissionmanagement.TeamType;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.start.ScapStartController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = OrganisationGroupController.class)
@WithMockUser
class OrganisationGroupControllerTest extends AbstractScapSubmitterControllerTest {

  @MockBean
  OrganisationGroupFormService organisationGroupFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;

  @MockBean
  OrganisationGroupService organisationGroupService;

  @MockBean
  ScapOperatorService scapOperatorService;

  static final Integer ORGANISATION_GROUP_ID = 1664;
  static final ServiceUserDetail USER = TestUserProvider.getUser();

  @Test
  void renderNewScapOrganisationGroupForm() throws Exception {
    var team = new Team();
    team.setDisplayName("org group name");
    team.setEnergyPortalOrgGroupId(1);

    when(userDetailService.getUserDetail()).thenReturn(USER);
    when(teamService.getTeamsOfTypeThatUserBelongsTo(USER, TeamType.INDUSTRY)).thenReturn(Collections.singletonList(team));

    mockMvc.perform(
        get(
            ReverseRouter.route(on(OrganisationGroupController.class).renderNewScapOrganisationGroupForm(null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/organisationGroup"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attributeExists("form"))
        .andExpect(model().attribute("permittedOrganisationGroups",
            Map.of(String.valueOf(team.getEnergyPortalOrgGroupId()), team.getDisplayName())));
  }

  @Test
  void saveNewScapOrganisationGroup_valid_verifyCreatesEntities() throws Exception {
    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveNewScapOrganisationGroup(null, emptyBindingResult()));
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID));
    var form = new OrganisationGroupForm();
    scap.setOrganisationGroupId(ORGANISATION_GROUP_ID);

    when(organisationGroupFormService.validate(any(OrganisationGroupForm.class), any(BindingResult.class)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));
    when(scapOperatorService.createScap(form)).thenReturn(scap);

    mockMvc.perform(
        post(postUrl)
            .flashAttr("form", form)
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapOperatorService).createScap(form);
  }

  @Test
  void saveNewScapOrganisationGroup_invalid() throws Exception {
    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveNewScapOrganisationGroup(null, emptyBindingResult()));
    var form = new OrganisationGroupForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("Error", "ErrorMessage", "default message"));
    var errorItems = List.of(new ErrorItem(
        1, "organisationGroupId", "Select who the operator is for this SCAP"));

    when(organisationGroupFormService.validate(any(), any())).thenReturn(bindingResult);
    when(validationErrorOrderingService.getErrorItemsFromBindingResult(any(), any())).thenReturn(errorItems);

    mockMvc.perform(
        post(postUrl).param("organisationGroupId", "")
            .with(csrf()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/organisationGroup"))
        .andExpect(model().attribute("errorItems", errorItems));

    verify(scapService, never()).createScap(any());
  }

  @Test
  void renderExistingScapOrganisationGroupForm() throws Exception {
    var organisationGroupId = 322;
    var organisationGroup = new OrganisationGroup(organisationGroupId, "CENTRICA", null, null, null, null);
    scap.setOrganisationGroupId(organisationGroupId);
    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(organisationGroupService.getOrganisationGroupById(organisationGroupId, "Get name of current SCAP operator"))
        .thenReturn(Optional.of(organisationGroup));
    when(organisationGroupFormService.getForm(scapDetail)).thenReturn(new OrganisationGroupForm());

    mockMvc.perform(
        get(
            ReverseRouter.route(on(OrganisationGroupController.class)
                .renderExistingScapOrganisationGroupForm(SCAP_ID))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/organisationGroup"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID))))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void saveExistingScapOrganisationGroup_valid() throws Exception {

    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveExistingScapOrganisationGroup(null, SCAP_ID, emptyBindingResult()));
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(SCAP_ID));
    var form = new OrganisationGroupForm();
    form.setOrganisationGroupId(ORGANISATION_GROUP_ID);
    var scap = new Scap(SCAP_ID);

    when(scapService.getScapById(SCAP_ID)).thenReturn(scap);
    when(scapDetailService.getLatestScapDetailByScapOrThrow(scap)).thenReturn(scapDetail);
    when(organisationGroupFormService.validate(any(OrganisationGroupForm.class), any(BindingResult.class)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));

    mockMvc.perform(
        post(postUrl)
            .with(csrf())
            .flashAttr("form", form))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapOperatorService).updateScapOperator(scap, scapDetail, form);
  }

  @Test
  void saveExistingScapOrganisationGroup_invalid() throws Exception {
    var scapId = 1;
    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveExistingScapOrganisationGroup(null, SCAP_ID, emptyBindingResult()));
    var form = new OrganisationGroupForm();
    var bindingResult = new BeanPropertyBindingResult(form, "form");
    bindingResult.addError(new FieldError("Error", "ErrorMessage", "default message"));
    var errorItems = List.of(new ErrorItem(
        1, "organisationGroupId", "Select who the operator is for this SCAP"));
    var scapOverview = new Scap(scapId);

    when(scapService.getScapById(scapId)).thenReturn(scapOverview);
    when(organisationGroupFormService.validate(any(), any())).thenReturn(bindingResult);
    when(validationErrorOrderingService.getErrorItemsFromBindingResult(any(), any())).thenReturn(errorItems);

    mockMvc.perform(
        post(postUrl).param("organisationGroupId", "")
            .with(csrf()))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/organisationGroup"))
        .andExpect(model().attribute("errorItems", errorItems));

    verify(scapService, never()).updateScapOrganisationGroup(any(), any());
  }

}
