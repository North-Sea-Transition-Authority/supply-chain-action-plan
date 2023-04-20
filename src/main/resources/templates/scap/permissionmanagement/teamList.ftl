
<#-- @ftlvariable name="errorList" type="java.util.List<uk.co.nstauthority.scap.fds.ErrorItem>" -->
<#-- @ftlvariable name="allTeams" type="java.util.List<uk.co.nstauthority.scap.permissionmanagement.TeamView>" -->
<#-- @ftlvariable name="pageTitle" type="java.lang.String" -->

<#include "../layout/layout.ftl">
<@defaultPage
  htmlTitle=pageTitle
  pageHeading=pageTitle
  errorItems=errorList
  pageSize=PageSize.FULL_WIDTH
  topNavigation=true
>
    <#if hasCreateTeamPermissions>
      <@fdsAction.link linkText="Create organisation group team" linkClass="govuk-button" linkUrl=springUrl(newTeamFormUrl)/>
    </#if>

  <table class="govuk-table">
    <thead class="govuk-table__head">
    <tr class="govuk-table__row">
      <th scope="col" class="govuk-table__header">Team</th>
      <th scope="col" class="govuk-table__header">Team type</th>
      <th scope="col" class="govuk-table__header">Actions</th>
    </tr>
    </thead>
    <tbody class="govuk-table__body">
      <#list allTeams as team>
        <tr class="govuk-table__row">
          <td class="govuk-table__cell">${team.DisplayName()}</td>
          <td class="govuk-table__cell">${team.teamType().getDisplayText()}</td>
          <td class="govuk-table__cell">
            <@fdsAction.link linkText="Manage" linkUrl=springUrl(team.manageUrl())/>
          </td>
        </tr>
      </#list>
    </tbody>
  </table>
</@defaultPage>
