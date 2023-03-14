<#-- @ftlvariable name="teamName" type="java.lang.String" -->
<#-- @ftlvariable name="teamRoles" type="java.util.List<uk.co.nstauthority.scap.permissionmanagement.regulator.RegulatorTeamRole>" -->
<#-- @ftlvariable name="teamMembers" type="java.util.List<uk.co.nstauthority.scap.permissionmanagement.TeamMemberView>" -->
<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#-- @ftlvariable name="backLinkUrl" type="java.lang.String" -->
<#-- @ftlvariable name="breadcrumbsList" type="java.util.Map<java.lang.String, java.lang.String>" -->
<#-- @ftlvariable name="canRemoveUsers" type="java.lang.Boolean" -->
<#-- @ftlvariable name="canEditUsers" type="java.lang.Boolean" -->
<#-- @ftlvariable name="addTeamMemberUrl" type="java.lang.String" -->
<#-- @ftlvariable name="removeTeamUrl" type="java.lang.String" -->
<#include "../layout/layout.ftl">
<#import '_teamMembers.ftl' as teamMembersMacro>
<#import '_roleDescriptions.ftl' as roleDescriptions>

<#assign pageTitle=teamName/>

<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  errorItems=errorList
  pageSize=PageSize.FULL_WIDTH
  topNavigation=true
>

  <@roleDescriptions.roleDescriptions roles=teamRoles/>
  <@fdsAction.buttonGroup>
    <#if addTeamMemberUrl?has_content>
      <@fdsAction.link
        linkText="Add user"
        linkUrl=springUrl(addTeamMemberUrl)
        linkClass="govuk-button govuk-button--secondary"
        role=true
      />
    </#if>
    <#if removeTeamUrl?has_content>
        <@fdsAction.link
        linkText="Archive Team"
        linkUrl=springUrl(removeTeamUrl)
        linkClass="govuk-button govuk-button--warning"
        role=true
        />
    </#if>
  </@fdsAction.buttonGroup>

  <#if teamMembers?has_content>
      <@teamMembersMacro.teamMembers name=teamName members=teamMembers canRemoveUsers=canRemoveUsers!false canEditUsers=canEditUsers!false/>
  <#else>
    <@fdsInsetText.insetText>
      ${teamName} has no members.
    </@fdsInsetText.insetText>
  </#if>

</@defaultPage>