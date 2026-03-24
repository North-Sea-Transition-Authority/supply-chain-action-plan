<#include "../layout/layout.ftl">
<#import '_teamMemberRoles.ftl' as teamMemberRoles>

<#-- @ftlvariable name="userDisplayName" type="java.lang.String" -->
<#-- @ftlvariable name="backLinkUrl" type="java.lang.String" -->
<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#-- @ftlvariable name="roles" type="java.util.Map<java.lang.String, java.lang.String>" -->

<@defaultPage
  htmlTitle= "What actions does ${userDisplayName} perform?"
  pageHeading=""
  errorItems=errorList
  pageSize=PageSize.TWO_THIRDS_COLUMN
  backLinkUrl=springUrl(backLinkUrl)
  topNavigation=true
>
  <#assign warning>
    <#if !userHasAllowedEmail>
      <@fdsWarning.warning>
        This user's email is not from an approved domain for this team.
      </@fdsWarning.warning>
    </#if>
  </#assign>

  <@fdsForm.htmlForm>
      <@fdsCheckbox.checkboxes
      fieldsetHeadingText="What actions does ${userDisplayName} perform?"
      fieldsetHeadingSize="h1"
      fieldsetHeadingClass="govuk-fieldset__legend--l"
      path="form.roles"
      checkboxes=roles
      hintText=warning
      />
    <@fdsAction.submitButtons
      primaryButtonText="Save and continue"
      secondaryLinkText="Cancel"
      linkSecondaryAction=true
      linkSecondaryActionUrl=springUrl(backLinkUrl)
    />
  </@fdsForm.htmlForm>
</@defaultPage>
