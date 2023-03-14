<#include '../layout/layout.ftl'>

<#macro serviceContact technicalSupport>
<#-- @ftlvariable name="serviceContact" type="uk.co.nstauthority.scap.technicalsupport.TechnicalSupportConfigurationProperties" -->
  <ul class="govuk-list">
    <li>${technicalSupport.name()}</li>
    <#if technicalSupport.phoneNumber()?has_content>
      <li>Telephone: ${technicalSupport.phoneNumber()}</li>
    </#if>
    <#if technicalSupport.emailAddress()?has_content>
      <li>Email: <@fdsAction.link linkText=technicalSupport.emailAddress() linkUrl="mailto:${technicalSupport.emailAddress()}"/></li>
    </#if>
  </ul>
</#macro>
