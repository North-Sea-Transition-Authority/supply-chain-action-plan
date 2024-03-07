<#include '../layout/layout.ftl'>

<#macro serviceContact serviceContactInfo>
  <ul class="govuk-list">
    <li>${serviceContactInfo.name()}</li>
    <#if serviceContactInfo.phoneNumber()?has_content>
      <li>Telephone: ${serviceContactInfo.phoneNumber()}</li>
    </#if>
    <#if serviceContactInfo.emailAddress()?has_content>
      <li>Email: <@fdsAction.link linkText=serviceContactInfo.emailAddress() linkUrl="mailto:${serviceContactInfo.emailAddress()}"/></li>
    </#if>
  </ul>
</#macro>
