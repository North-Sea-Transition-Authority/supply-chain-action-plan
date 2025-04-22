<#include '../../fds/objects/layouts/generic.ftl'>

<#macro teamMembers name members canEditOrRemoveUsers>
  <table class="govuk-table">
    <caption class="govuk-table__caption govuk-table__caption--m govuk-visually-hidden">Members of ${name}</caption>
    <thead class="govuk-table__head">
      <tr class="govuk-table__row">
        <th scope="col" class="govuk-table__header">Name</th>
        <th scope="col" class="govuk-table__header">Contact details</th>
        <th scope="col" class="govuk-table__header">Roles</th>
          <#if canEditOrRemoveUsers>
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
                <li>${member.contactEmail()}</li>
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
          <#if canEditOrRemoveUsers>
            <td class="govuk-table__cell">
              <ul class="govuk-list govuk-!-margin-bottom-0">
                <li >
                  <@fdsAction.link linkText="Edit" linkUrl=springUrl(member.editUrl()) linkScreenReaderText=member.getDisplayName()/>
                </li>
                <li >
                  <@fdsAction.link linkText="Remove" linkUrl=springUrl(member.removeUrl()) linkScreenReaderText=member.getDisplayName() />
                </li>
              </ul>
            </td>
          </#if>
      </tr>
    </#list>
    </tbody>
  </table>
</#macro>
