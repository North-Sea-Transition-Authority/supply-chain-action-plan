<#include '../../fds/objects/layouts/generic.ftl'>

<#macro teamMembers name members canRemoveUsers=false canEditUsers=false>
  <#assign showActionColumn = canRemoveUsers || canEditUsers/>
  <table class="govuk-table">
    <caption class="govuk-table__caption govuk-table__caption--m govuk-visually-hidden">Members of ${name}</caption>
    <thead class="govuk-table__head">
      <tr class="govuk-table__row">
        <th scope="col" class="govuk-table__header">Name</th>
        <th scope="col" class="govuk-table__header">Contact details</th>
        <th scope="col" class="govuk-table__header">Roles</th>
          <#if showActionColumn>
            <th scope="col" class="govuk-table__header">Actions</th>
          </#if>
      </tr>
    </thead>
    <tbody class="govuk-table__body">
      <#list members as member>
        <tr class="govuk-table__row">
          <td class="govuk-table__cell">${member.getDisplayName()}</td>
          <td class="govuk-table__cell">
            <ul class="govuk-list govuk-!-margin-bottom-0">
              <#if member.contactEmail()?has_content>
                <li>
                </li>
              </#if>
              <#if member.contactNumber()?has_content>
                <li>${member.contactNumber()}</li>
              </#if>
            </ul>
          </td>
          <td class="govuk-table__cell">
            <ul class="govuk-list govuk-!-margin-bottom-0">
              <#list member.teamRoles() as role>
                <li>${role.getDisplayName()}</li>
              </#list>
          </ul>
        </td>
          <#if showActionColumn>
            <td class="govuk-table__cell">
              <#if canEditUsers>
                <ul class="govuk-list govuk-!-margin-bottom-0"><@fdsAction.link linkText="Edit" linkUrl=springUrl(member.editUrl()) linkScreenReaderText=member.getDisplayName() /></ul>
              </#if>
              <#if canRemoveUsers>
                <ul class="govuk-list govuk-!-margin-bottom-0"><@fdsAction.link linkText="Remove" linkUrl=springUrl(member.removeUrl()) linkScreenReaderText=member.getDisplayName() /></ul>
              </#if>
            </td>
          </#if>
      </tr>
    </#list>
    </tbody>
  </table>
</#macro>
