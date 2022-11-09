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

import java.util.List;
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
import uk.co.nstauthority.scap.AbstractControllerTest;
import uk.co.nstauthority.scap.fds.ErrorItem;
import uk.co.nstauthority.scap.mvc.ReverseRouter;
import uk.co.nstauthority.scap.scap.detail.ScapDetailService;
import uk.co.nstauthority.scap.scap.scap.Scap;
import uk.co.nstauthority.scap.scap.scap.ScapService;
import uk.co.nstauthority.scap.scap.start.ScapStartController;
import uk.co.nstauthority.scap.scap.tasklist.TaskListController;
import uk.co.nstauthority.scap.validation.ValidationErrorOrderingService;

@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes = OrganisationGroupController.class)
@WithMockUser
class OrganisationGroupControllerTest extends AbstractControllerTest {

  @MockBean
  ScapService scapService;

  @MockBean
  ScapDetailService scapDetailService;

  @MockBean
  OrganisationGroupFormService organisationGroupFormService;

  @MockBean
  ValidationErrorOrderingService validationErrorOrderingService;

  @MockBean
  OrganisationGroupService organisationGroupService;

  static final Integer ORGANISATION_GROUP_ID = 1664;

  @Test
  void renderNewScapOrganisationGroupForm() throws Exception {
    mockMvc.perform(
        get(
            ReverseRouter.route(on(OrganisationGroupController.class).renderNewScapOrganisationGroupForm(null))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/organisationGroup"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(ScapStartController.class).renderStartNewScap())))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(OrganisationGroupController.class).saveNewScapOrganisationGroup(null, emptyBindingResult()))))
        .andExpect(model().attribute("organisationGroupSearchRestUrl",
            ReverseRouter.route(on(OrganisationGroupRestController.class).getOrganisationGroupSearchResults(null))))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void saveNewScapOrganisationGroup_valid_verifyCreatesEntities() throws Exception {
    var scapId = 234;
    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveNewScapOrganisationGroup(null, emptyBindingResult()));
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId));
    var form = new OrganisationGroupForm();
    var scap = new Scap(scapId);
    scap.setOrganisationGroupId(ORGANISATION_GROUP_ID);

    when(scapService.createScapOverview(ORGANISATION_GROUP_ID))
        .thenReturn(scap);
    when(organisationGroupFormService.validate(any(OrganisationGroupForm.class), any(BindingResult.class)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));
    when(scapService.createScapOverview(ORGANISATION_GROUP_ID)).thenReturn(scap);

    mockMvc.perform(
        post(postUrl).param("organisationGroupId", String.valueOf(ORGANISATION_GROUP_ID))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapService).createScapOverview(ORGANISATION_GROUP_ID);
    verify(scapDetailService).createDraftScapDetail(scap);
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

    verify(scapService, never()).createScapOverview(any());
  }

  @Test
  void renderExistingScapOrganisationGroupForm() throws Exception {
    var organisationGroupId = 322;
    var scapId = 1;
    var organisationGroup = new OrganisationGroup(organisationGroupId, "CENTRICA", null, null, null, null);
    var scapOverview = new Scap(scapId);
    scapOverview.setOrganisationGroupId(organisationGroupId);
    when(scapService.getScapById(scapId)).thenReturn(scapOverview);
    when(organisationGroupService.getOrganisationGroupById(organisationGroupId, "Get name of current SCAP operator"))
        .thenReturn(Optional.of(organisationGroup));
    when(organisationGroupFormService.getForm(scapOverview)).thenReturn(new OrganisationGroupForm());

    mockMvc.perform(
        get(
            ReverseRouter.route(on(OrganisationGroupController.class)
                .renderExistingScapOrganisationGroupForm(scapId))))
        .andExpect(status().isOk())
        .andExpect(view().name("scap/scap/organisationGroup"))
        .andExpect(model().attribute("backLinkUrl",
            ReverseRouter.route(on(TaskListController.class).renderTaskList(1))))
        .andExpect(model().attribute("submitPostUrl",
            ReverseRouter.route(on(OrganisationGroupController.class)
                .saveExistingScapOrganisationGroup(null, 1, emptyBindingResult()))))
        .andExpect(model().attribute("organisationGroupSearchRestUrl",
            ReverseRouter.route(on(OrganisationGroupRestController.class)
                .getOrganisationGroupSearchResults(null))))
        .andExpect(model().attributeExists("form"));
  }

  @Test
  void saveExistingScapOrganisationGroup_valid() throws Exception {
    var scapId = 1;
    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveExistingScapOrganisationGroup(null, scapId, emptyBindingResult()));
    var expectedRedirectUrl = ReverseRouter.route(on(TaskListController.class).renderTaskList(scapId));
    var form = new OrganisationGroupForm();
    var scapOverview = new Scap(scapId);

    when(scapService.getScapById(scapId)).thenReturn(scapOverview);
    when(organisationGroupFormService.validate(any(OrganisationGroupForm.class), any(BindingResult.class)))
        .thenReturn(new BeanPropertyBindingResult(form, "form"));

    mockMvc.perform(
        post(postUrl)
            .param("organisationGroupId", String.valueOf(ORGANISATION_GROUP_ID))
            .with(csrf()))
        .andExpect(status().is3xxRedirection())
        .andExpect(view().name(String.format("redirect:%s", expectedRedirectUrl)));

    verify(scapService)
        .updateScapOverviewOrganisationGroup(scapOverview, ORGANISATION_GROUP_ID);
  }

  @Test
  void saveExistingScapOrganisationGroup_invalid() throws Exception {
    var scapId = 1;
    var postUrl = ReverseRouter.route(on(OrganisationGroupController.class)
        .saveExistingScapOrganisationGroup(null, scapId, emptyBindingResult()));
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

    verify(scapService, never()).updateScapOverviewOrganisationGroup(any(), any());
  }

}
